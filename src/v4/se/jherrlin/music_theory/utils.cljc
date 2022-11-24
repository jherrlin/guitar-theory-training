(ns v4.se.jherrlin.music-theory.utils
  "

  Index tones are sets:        #{:db :c#}
  Interval tones are keywords: :c#"
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [se.jherrlin.utils :as utils]
   [malli.destructure :as md]
   [malli.provider :as mp]
   [malli.core :as m]
   [v4.se.jherrlin.music-theory.intervals :as intervals]
   [v4.se.jherrlin.music-theory.models.chord :as models.chord]
   [v4.se.jherrlin.music-theory.models.scale :as models.scale]
   [v4.se.jherrlin.music-theory.models.fretboard-pattern :as models.fretboard-pattern]))


;; Terms
;; fretboard-matrix

(def all-tones [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])


(defn fretboard-string
  "Generate a freatboard string.

  `all-tones`        - A collection of all the tones
  `tuning`           - Tuning on string
  `number-of-frets`  - Width of the freatboard"
  [all-tones string-tune number-of-frets]
  (->> (mapv
        (fn [x t]
          {:x          x
           :tone       t})
        (iterate inc 0)
        (->> (utils/rotate-until #(% string-tune) all-tones)
             (cycle)
             (take number-of-frets)))))

(fretboard-string
 all-tones
 :e
 12)


(defn fretboard-strings
  "Generate freatboard matrix.

  `all-tones`        - A collection of all the tones
  `tunings`          - Tuning on strings
  `number-of-frets`  - Width of the freatboard"
  [all-tones string-tunes number-of-frets]
  (->> string-tunes
       (mapv
        (fn [y string-tune]
          (mapv
           #(assoc % :y y)
           (fretboard-string all-tones string-tune number-of-frets)))
        (iterate inc 0))))

(fretboard-strings
 all-tones
 [:e :b :g :d :a :e]
 2)



(defn gen-fretboard-matrix
  "Generate a matrix that represents the fretboard.

  `all-tones`        - A collection of all the tones
  `tunings`          - Tunings on each string
  `number-of-frets`  - Width of the freatboard"
  [all-tones tunings number-of-frets]
  (->> tunings
       (mapv
        (fn [y string-tune]
          (mapv
           #(assoc % :y y)
           (fretboard-string all-tones string-tune number-of-frets)))
        (iterate inc 0))))

(gen-fretboard-matrix
 all-tones
 [:e :b :g :a :b]
 3)



(defn sharp-or-flat
  "Select tone from interval.
  Tone is a set: #{:db :c#}
  Interval is a string: \"3#\""
  [tone interval]
  {:pre [(set? tone)]}
  (-> (cond
        (= 1 (count tone))           tone
        (str/includes? interval "b") (filter (comp #(str/includes? % "b") name) tone)
        :else                        (filter (comp #(str/includes? % "#") name) tone))
      (first)))

(sharp-or-flat
 #{:db :c#}
 "#3")

(sharp-or-flat
 #{:db :c#}
 "b3")


(defn tones-on-indexes-with-intervals-2 [indexes intervals tones]
  (mapv
   (fn [index interval]
     (merge
      (get intervals/intervals-map-by-function interval)
      {:tone     (sharp-or-flat (nth tones index) interval)
       :interval interval
       :index    index}))
   indexes
   intervals))

(tones-on-indexes-with-intervals-2
 [0 3 7]
 ["1" "b3" "5"]
 all-tones)

(defn tones-on-indexes-with-intervals [indexes intervals tones]
  (mapv
   (fn [index interval]
     (sharp-or-flat (nth tones index) interval))
   indexes
   intervals))

(tones-on-indexes-with-intervals
 [0 3 7]
 ["1" "b3" "5"]
 all-tones)



(defn tones-by-indexes [tones indexes]
  (utils/take-indexes tones indexes))


(defn tones-by-intervals
  "Get tones from intervals

  `intervals`   -   [\"1\" \"3\" \"5\"]
  =>
  [:c :e :g]"
  [all-tones intervals]
  (->> intervals
       (mapv (fn [interval-function]
               (let [interval-index (get-in intervals/intervals-map-by-function [interval-function :semitones])]
                 (sharp-or-flat (nth all-tones interval-index) interval-function))))))


(tones-by-intervals
 all-tones
 ["1" "3" "5"]
 )

(defn tones-by-key-and-intervals
  "

  `key-of`    -  :c
  `intervals` -  [\"1\" \"3\" \"5\"]
  =>
  [:c :e :g]"
  [all-tones key-of intervals]
  (tones-by-intervals
   (utils/rotate-until #(% key-of) all-tones)
   intervals))

(tones-by-key-and-intervals
 all-tones
 :c
 #_["1" "b3" "5"]
 ["1" "3" "5"])
;;



(defn tones-by-key-and-intervals-2
  "

  `key-of`    -  :c
  `intervals` -  [\"1\" \"3\" \"5\"]
  =>
  [:c :e :g]"
  [all-tones key-of intervals]
  (tones-on-indexes-with-intervals-2
   (->> intervals
        (mapv (fn [interval-function]
                (get-in intervals/intervals-map-by-function [interval-function :semitones]))))
   intervals
   (utils/rotate-until #(% key-of) all-tones)
   ))

(tones-by-key-and-intervals-2
 all-tones
 :c
 ["1" "b3" "5"]
 #_["1" "3" "5"])

[{:semitones 0,
  :function "1",
  :name/en "Root",
  :name/sv "Root",
  :tone :c,
  :interval "1",
  :index 0}
 {:semitones 3,
  :function "b3",
  :name/en "Minor third",
  :name/sv "Moll-ters",
  :tone :eb,
  :interval "b3",
  :index 3}
 {:semitones 7,
  :function "5",
  :name/en "Perfect fifth",
  :name/sv "Kvint",
  :tone :g,
  :interval "5",
  :index 7}]


(defn intevals-string->intervals-matrix
  [interval]
  (->> interval
       (str/trim)
       (str/split-lines)
       (mapv (fn [line]
               (->> line
                    str/trim
                    (re-seq #"(b{0,2}#{0,2}\d{1,2})|-")
                    (mapv second))))))

(intevals-string->intervals-matrix
 "   3   -   -
   -   bb1   -
   5   -   -
   -   -   -
   -   -   -
   -   -   -")




(defn find-fretboard-pattern
  "Find patterns on the "
  [all-tones key-of interval-matrix fretboard-matrix]
  (let [
        ;; interval-matrix       (trim-matrix interval-matrix)
        interval-matrix-width (-> interval-matrix first count)
        fretboard-count       (-> fretboard-matrix first count)]
    (loop [counter           0
           found-pattern-xys #{}]
      (let [combinations-p
            (->>  fretboard-matrix
                  (mapv (comp vec (partial take interval-matrix-width) (partial drop counter))))
            combinations
            (->> combinations-p
                 (apply concat)
                 (mapv vector (apply concat interval-matrix)))
            box-match? (->> combinations
                            (remove (comp nil? first))
                            (every? (fn [[interval' {:keys [tone] :as tone'}]]
                                      (let [interval-semitones (get-in intervals/intervals-map-by-function [interval' :semitones])
                                            fretboard-tone     (nth
                                                                (utils/rotate-until #(% key-of) all-tones)
                                                                interval-semitones)]
                                        (and
                                         (= (-> combinations-p first count)
                                            interval-matrix-width)
                                         (= tone fretboard-tone))))))
            pattern-check
            (->> combinations
                 (remove (comp nil? first))
                 (map (fn [[interval' {:keys [tone] :as tone'}]]
                        (let [interval-semitones (get-in intervals/intervals-map-by-function [interval' :semitones])
                              fretboard-tone     (nth
                                                  (utils/rotate-until #(% key-of) all-tones)
                                                  interval-semitones)]
                          (assoc tone'
                                 :match? (and box-match? (= tone fretboard-tone))
                                 :interval interval')))))
            found-pattern-xys'
            (if-not box-match?
              found-pattern-xys
              (->> pattern-check
                   (filter :match?)
                   (map #(select-keys % [:x :y :interval]))
                   (set)
                   (into found-pattern-xys)))]
        (if (< counter fretboard-count)
          (recur
           (inc counter)
           found-pattern-xys')
          (->> fretboard-matrix
               (apply concat)
               (map (fn [{:keys [x y] :as m}]
                      (if-not (contains? (->> found-pattern-xys'
                                              (map (juxt :x :y))
                                              (set))
                                         [x y])
                        m
                        (assoc m
                               :pattern-match? true
                               :interval (->> found-pattern-xys'
                                              (filter (fn [{x' :x y' :y}]
                                                        (= [x y] [x' y'])))
                                              (first)
                                              :interval)))))
               (partition fretboard-count)
               (mapv #(mapv identity %))))))))

(find-fretboard-pattern
 all-tones
 :e
 [["1" nil nil]
  ["5" nil nil]
  ["b3" nil nil]
  [nil nil "1"]
  [nil nil "5"]
  ["1" nil nil]]
 (fretboard-strings
  all-tones
  [:e :b :g :d :a :e]
  4))



(defn match-chord-with-scales
  "Find what scales that works with a chord, by the chord indexes.

  `scales-map`     - Map with scales
  `chord-indexes`  - Seq with chord indexes, example: `[0 4 7]`"
  [scales-map chord-indexes]
  (->> scales-map
       (vals)
       (filter (fn [scale]
                 (let [scale-indexes (get scale :scale/indexes)]
                   (set/subset? (set chord-indexes) (set scale-indexes)))))))

(match-chord-with-scales
 {:ionian
  #:scale{:id        :ionian,
          :intervals ["1" "2" "3" "4" "5" "6" "7"],
          :indexes   [0 2 4 5 7 9 11],
          :title     "ionian",
          :order     4}}
 [0 4 7])




(defn find-chords [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (utils/rotate-until
                         #(if (set? root-tone)
                            (= % root-tone)
                            (% root-tone))
                         all-tones)]
    (->> chords-map
         (vals)
         (filter (fn [{:chord/keys [indexes]}]
                   (let [chord-to-serch (tones-by-indexes tones indexes)
                         chord-to-match chord-tones]
                     (and (= (count chord-to-serch) (count chord-to-match))
                          (->> (map (fn [a b]
                                      (if (set? b)
                                        (= a b)
                                        (boolean (a b))))
                                chord-to-serch
                                chord-to-match)
                               (every? true?)))))))))

(find-chords
 {:major
  #:chord{:id           :major,
          :intervals    ["1" "3" "5"],
          :indexes      [0 4 7],
          :title        "major",
          :order        1,
          :sufix        "",
          :explanation  "major",
          :display-text "major"}
  :minor
  #:chord{:id           :minor,
          :intervals    ["1" "b3" "5"],
          :indexes      [0 3 7],
          :title        "minor",
          :order        2,
          :sufix        "m",
          :explanation  "minor",
          :display-text "minor"}}
 all-tones
 #_[#{:c} #{:e} #{:g}]
 #_[#{:c} #{:d# :eb} #{:g}]
 [:c :e :g]
 )




(defn find-chord [chords-map all-tones chord-tones]
  (->> (find-chords chords-map all-tones chord-tones)
       (first)))

(find-chord
 {:major
  #:chord{:id           :major,
          :intervals    ["1" "3" "5"],
          :indexes      [0 4 7],
          :title        "major",
          :order        1,
          :sufix        "",
          :explanation  "major",
          :display-text "major"}}
 all-tones
 [#{:c} #{:e} #{:g}])




;; Doesnt work yet
;; (defn chord-name
;;   [all-tones chords-map chord-tones]
;;   (let [root-tone             (first chord-tones)
;;         {:chord/keys [sufix]} (find-chord chords-map all-tones chord-tones)]
;;     (str (-> root-tone name str/lower-case str/capitalize) sufix)))

;; (chord-name
;;  all-tones
;;  {:major
;;   #:chord{:id           :major,
;;           :intervals    ["1" "3" "5"],
;;           :indexes      [0 4 7],
;;           :title        "major",
;;           :order        1,
;;           :sufix        "",
;;           :explanation  "major",
;;           :display-text "major"}}
;;  [#{:c} #{:e} #{:g}])




(defn add-layer [f fretboard-matrix]
  (utils/map-matrix
   (fn [x]
     (if-let [x' (f x)]
       (assoc x :out x')
       x))
   fretboard-matrix))

(defn add-pattern
  [{:keys [x y tone pattern-match? interval] :as m}]
  (when (seq interval)
    (-> (sharp-or-flat tone interval) name str/capitalize)))

(defn add-sharps
  [{:keys [x y tone pattern-match? interval] :as m}]
  (-> tone (sharp-or-flat "#") name str/capitalize))

(defn add-flats
  [{:keys [x y tone pattern-match? interval] :as m}]
  (-> tone (sharp-or-flat "b") name str/capitalize))

(defn add-chord-tones
  [chord-tones {:keys [x y tone pattern-match? interval] :as m}]
  (when-let [tone' (first (set/intersection (set chord-tones) tone))]
    (-> tone' name str/capitalize)))

(defn add-intervals
  "Show as intervals.

  Example:
  chord-tones-and-intervals: `[[:c \"1\"] [:d \"2\"] [:e \"3\"] [:f \"4\"] [:g \"5\"] [:a \"6\"] [:b \"7\"]]`"
  [chord-tones-and-intervals {:keys [x y tone pattern-match? interval] :as m}]
  (when-let [i (->> chord-tones-and-intervals
                    (filter (fn [[tone' interval']]
                              (tone tone')))
                    (first)
                    (second))]
    i))

(defn add-pattern-with-intervals
  [{:keys [x y tone pattern-match? interval] :as m}]
  (when (and pattern-match? interval)
    interval))


(->> (find-fretboard-pattern
      all-tones
      :e
      [["1" nil nil]
       ["5" nil nil]
       ["b3" nil nil]
       [nil nil "1"]
       [nil nil "5"]
       ["1" nil nil]]
      (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       12))
     (add-layer
      #_add-flats
      #_add-sharps
      add-pattern))

(->> (fretboard-strings
      all-tones
      [:e :b :g :d :a :e]
      10)
     (add-layer
      #_(partial add-chord-tones [:e :b :g])
      (partial add-intervals [[:e "1"] [:b "b3"] [:g "5"]])))


(defn fretboard-str
  [tone-f matrix]
  (let [add-table-stuff
        (fn [row]
          (str "|" (apply str (interpose "|" (map #(utils/fformat " %-3s" %) row))) "|"))
        rows
        (->> matrix
             (map
              (fn [row]
                (->> row
                     (map tone-f))))
             (map add-table-stuff))
        row-length (-> rows first count)]
    (->> rows
         (utils/list-insert (add-table-stuff (->> matrix first (map :x))) 0)
         (utils/list-insert (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
         (str/join "\n"))))


(->> (fretboard-strings
      all-tones
      [:e :b :g :d :a :e]
      12)
     (add-layer
      #_(partial add-chord-tones [:e :b :g])
      (partial add-intervals [[:e "1"] [:b "b3"] [:g "5"]]))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))

(->> (find-fretboard-pattern
      all-tones
      :e
      [["1" nil nil]
       ["5" nil nil]
       ["b3" nil nil]
       [nil nil "1"]
       [nil nil "5"]
       ["1" nil nil]]
      (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       10))
     (add-layer
      #_add-flats
      #_add-sharps
      #_add-pattern
      (partial add-intervals [[:e "1"] [:b "b3"] [:g "5"]])
      )
     ;; (trim-matrix #(every? nil? (map :out %))) ;; Trim fretboard
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))


(->> (find-fretboard-pattern
      all-tones
      :c
      [["5" nil nil]
       [nil nil "3"]
       [nil nil "1"]
       [nil nil "5"]
       ["1" nil nil]
       ["5" nil nil]]
      (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       10))
     (add-layer
      #_add-flats
      #_add-sharps
      add-pattern)
     ;; (trim-matrix #(every? nil? (map :out %))) ;; Trim fretboard
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))



;; Public functions

(defn pattern-with-intervals
  [key-of pattern fretboard-matrix]
  (->> (find-fretboard-pattern
        all-tones
        key-of
        pattern
        fretboard-matrix)
       (add-layer add-pattern-with-intervals)))

(->> (pattern-with-intervals
      :a
      [["5" nil nil]
       [nil nil "3"]
       [nil nil "1"]
       [nil nil "5"]
       ["1" nil nil]
       ["5" nil nil]]
      (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       10))
     (utils/trim-matrix #(every? nil? (map :out %)))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))



(defn pattern-with-tones
  [key-of pattern fretboard-matrix]
  (->> (find-fretboard-pattern
        all-tones
        key-of
        pattern
        fretboard-matrix)
       (add-layer add-pattern)))

(->> (pattern-with-tones
      :c
      [["5" nil nil]
       [nil nil "3"]
       [nil nil "1"]
       [nil nil "5"]
       ["1" nil nil]
       ["5" nil nil]]
      (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       10))
     (utils/trim-matrix #(every? nil? (map :out %)))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))



(defn with-all-tones
  "
  `tones` - `[:e :b :g]`"
  [tones fretboard-matrix]
  (->> fretboard-matrix
       (add-layer (partial add-chord-tones tones))))

(->> (with-all-tones
       [:e :b :g]
       (fretboard-strings
        all-tones
        [:e :b :g :d :a :e]
        10))
     (utils/trim-matrix #(every? nil? (map :out %)))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))



(defn with-all-intervals
  "
  chord-tones-and-intervals: `[[:c \"1\"] [:d \"2\"] [:e \"3\"] [:f \"4\"] [:g \"5\"] [:a \"6\"] [:b \"7\"]]`"
  [chord-tones-and-intervals fretboard-matrix]
  (->> fretboard-matrix
       (add-layer (partial add-intervals chord-tones-and-intervals))))

(->> (with-all-intervals
       [[:e "1"] [:b "b3"] [:g "5"]]
       (fretboard-strings
        all-tones
        [:e :b :g :d :a :e]
        12))
     (utils/trim-matrix #(every? nil? (map :out %)))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))


(defn with-all-sharps
  [fretboard-matrix]
  (->> fretboard-matrix
       (add-layer add-sharps)))


(->> (with-all-sharps
       (fretboard-strings
        all-tones
        [:e :b :g :d :a :e]
        12))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))


(defn with-all-flats
  [fretboard-matrix]
  (->> fretboard-matrix
       (add-layer add-flats)))


(->> (with-all-flats
       (fretboard-strings
        all-tones
        [:e :b :g :d :a :e]
        12))
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))



;; Harmonization

(def triad   (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def seventh (juxt #(nth % 0) #(nth % 2) #(nth % 4) #(nth % 6)))

(defn gen-harmonization [scales chords key-of scale' steps-fn]
  (let [scale         (get scales scale')
        scale-indexes (get scale :scale/indexes)
        scale-tones   (tones-by-indexes
                       (utils/rotate-until
                        #(% key-of)
                        all-tones)
                       scale-indexes)]
  (->> scale-tones
       (mapv
        (fn [t]
          (let [index-chord-tones (steps-fn
                                   (utils/rotate-until #(= % t) scale-tones))
                found-chord       (find-chord
                                   chords
                                   all-tones
                                   index-chord-tones)
                interval-tones    (mapv (fn [interval' index']
                                          (sharp-or-flat index' interval'))
                                        (:chord/intervals found-chord)
                                        index-chord-tones)
                ]
            (assoc
             found-chord
             :key-of key-of
             :chord/index-tones index-chord-tones
             :chord/interval-tones interval-tones
             :chord/root-tone (first interval-tones)
             ))))

       (mapv
        #(assoc %7
                :harmonization/index      %1
                :harmonization/position   %2
                :harmonization/mode       %3
                :harmonization/mode-str   %4
                :harmonization/family     %5
                :harmonization/family-str %6)
        (range 1 100)
        (if (= scale :major)
          ["I" "ii" "iii" "IV" "V" "vi" "vii"]
          ["i" "ii" "III" "iv" "v" "VI" "VII"])
        (if (= scale :major)
          [:ionian  :dorian  :phrygian :lydian :mixolydian :aeolian :locrian]
          [:aeolian :locrian :ionian   :dorian :phrygian   :lydian  :mixolydian])
        (if (= scale :major)
          ["Ionian"  "Dorian"  "Phrygian" "Lydian" "Mixolydian" "Aeolian" "Locrian"]
          ["Aeolian" "Locrian" "Ionian"   "Dorian" "Phrygian"   "Lydian"  "Mixolydian"])
        (if (= scale :major)
          [:tonic :subdominant :tonic :subdominant :dominant :tonic :dominant]
          [:tonic :subdominant :tonic :subdominant :dominant :subdominant :dominant])
        (if (= scale :major)
          ["T" "S" "T" "S" "D" "T" "D"]
          ["T" "S" "T" "S" "D" "S" "D"])))))


(comment
  (gen-harmonization
   @v4.se.jherrlin.music-theory.definitions/scales
   @v4.se.jherrlin.music-theory.definitions/chords
   :c
   :minor
   triad #_seventh)
  )

{:minor
 #:scale{:id        :minor,
         :intervals ["1" "2" "b3" "4" "5" "b6" "b7"],
         :indexes   [0 2 3 5 7 8 10],
         :title     "minor",
         :order     2}
 :major
 #:scale{:id        :major,
         :intervals ["1" "2" "3" "4" "5" "6" "7"],
         :indexes   [0    2   4   5   7   9  11],
         :title     "major",
         :order     1}}


;; REPL stuff below line




(let [tones     [:d :f# :a]
      tones-set (set tones)]
  (->> all-tones
       (utils/rotate-until #(% :d#))
       (map (fn [tone]
              (cond-> {:tone tone}
                (seq (set/intersection tones-set tone))
                (assoc :match true))))))

(set/intersection #{:d :f# :a} #{:gb :f#})
(set/intersection #{:d :f# :a} #{:gb})













;; Find the chord, cartesian product of all chords and key-of
;; (->> (for [{:chord/keys [intervals] :as chord} (vals @v2.se.jherrlin.music-theory.definitions/chords-atom)
;;            tone                                (apply concat all-tones)]
;;        (let [tones (intervals->tones
;;                     all-tones
;;                     sharp-or-flat
;;                     rotate-until
;;                     tone
;;                     intervals)]
;;          (assoc chord :key-of tone :tones (set tones))))
;;      (group-by :tones)
;;      ;; (map (fn [[a b]] [a (count b)]))
;;      ;; (sort-by  second #(compare %2 %1))
;;      )




(defn define-chord
  ([chord-id intervals-str]
   (define-chord chord-id {} intervals-str))
  ([chord-id {:keys [sufix] :as meta-data} intervals-str]
   {:pre [(keyword? chord-id) (map? meta-data) (string? intervals-str)]}
   (let [intervals' (->> (re-seq (re-pattern models.chord/regex) intervals-str)
                         (vec))
         indexes    (intervals/functions-to-semitones intervals')
         name'      (-> chord-id
                        name
                        (str/replace "-" " "))
         chord      (merge
                     {:chord/id        chord-id
                      :chord/intervals intervals'
                      :chord/indexes   indexes
                      :chord/name      name'
                      :chord/sufix     sufix}
                     (->> meta-data
                          (map (fn [[k v]]
                                 [(->> k name (str "chord/") keyword) v]))
                          (into {})))]
     (if (models.chord/valid-chord? chord)
       chord
       (throw (ex-info "Chord is not valid" (models.chord/explain-chord chord)))))))

(define-chord :major
  {:sufix        ""
   :explanation  "major"
   :display-text "major"}
  "1 3 5")



(defn define-scale
  ([scale-id intervals-str]
   (define-scale scale-id {} intervals-str))
  ([scale-id meta-data intervals-str]
   (let [intervals' (->> (re-seq (re-pattern models.chord/regex) intervals-str)
                         (vec))
         indexes    (intervals/functions-to-semitones intervals')
         scale      (merge
                     {:scale/id        scale-id
                      :scale/intervals intervals'
                      :scale/indexes   indexes
                      :scale/name      (-> scale-id
                                           name
                                           (str/replace "-" " "))}
                     (->> meta-data
                          (map (fn [[k v]]
                                 [(->> k name (str "scale/") keyword) v]))
                          (into {})))]
     (if (models.scale/valid-scale? scale)
       scale
       (throw (ex-info "Scale is not valid" (models.scale/explain-scale scale)))))))

(define-scale :major
  "1, 2, 3, 4, 5, 6, 7")



(defn define-pattern
  ([pattern-id pattern]
   (define-pattern pattern-id {} pattern))
  ([pattern-id {:keys [tuning type] :as meta-data} pattern]
   (let [pattern'   (->> pattern
                         (intevals-string->intervals-matrix)
                         (utils/trim-matrix))
         meta-data  (->> meta-data
                         (map (fn [[k v]]
                                [(->> k name (str "fretboard-pattern/") keyword) v]))
                         (into {}))
         inversion? (->> pattern'
                         (reverse)
                         (apply concat)
                         (remove nil?)
                         (first)
                         (= "1")
                         (not))
         pattern*   (merge
                     meta-data
                     {:fretboard-pattern/id         pattern-id
                      :fretboard-pattern/type       type
                      :fretboard-pattern/tuning     tuning
                      :fretboard-pattern/pattern    pattern'
                      :fretboard-pattern/str        pattern
                      :fretboard-pattern/inversion? inversion?})]
     (if (models.fretboard-pattern/validate-fretboard-pattern? pattern*)
       pattern*
       (throw (ex-info "Pattern is not valid" (models.fretboard-pattern/explain-fretboard-pattern pattern*)))))))

(define-pattern :major-1
  {:name   :major
   :chord  :major
   :tuning [:e :b :g :d :a :e]
   :type   :chord}
  "3   -
   -   1
   5   -
   -   -
   -   -
   -   -")
