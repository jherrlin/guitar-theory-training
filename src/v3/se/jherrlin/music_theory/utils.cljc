(ns v3.se.jherrlin.music-theory.utils
  (:require
   [v2.se.jherrlin.music-theory.intervals :as intervals]
   [v2.se.jherrlin.music-theory.utils :as utils-v2]
   [clojure.set :as set]
   [clojure.string :as str]
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])))


;; Terms
;; fretboard-matrix

(def all-tones [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])


(defn list-insert [elem index lst]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))

(list-insert
 0
 3
 [1 2 3 4])


#?(:cljs
   (defn fformat
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args))
   :clj (def fformat format))

(defn rotate-until
  "Rotate collection `xs` util `pred`."
  [pred xs]
  {:pre [(fn? pred) (coll? xs)]}
  (let [xs-count (count xs)]
    (->> (cycle xs)
         (drop-while #(not (pred %)))
         (take xs-count)
         (vec))))

(rotate-until
 #(% :f#)
 all-tones)



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
        (->> (rotate-until #(% string-tune) all-tones)
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
 [:e :f#]
 16)



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
 "3#")



(defn juxt-indexes
  "Create a juxt function that takes collection indexes and returns the value in
  indexes when applied."
  [indexes]
  (apply juxt
         (map
          (fn [index]
            (fn [tones]
              (nth tones index)))
          indexes)))

((juxt-indexes
  [0 3 5])
 [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f}])



(defn juxt-indexes-and-intervals
  "Create a juxt function that takes a collection of indexes and intervals and
  returns the tones."
  [indexes intervals]
  (apply juxt
         (map
          (fn [index interval]
            (fn [tones]
              (sharp-or-flat
               (nth tones index)
               interval)))
          indexes
          intervals)))

((juxt-indexes-and-intervals
  [0 3 7]
  ["1" "b3" "5"])
 all-tones)



(defn tones-on-indexes-with-intervals [indexes intervals tones]
  ((juxt-indexes-and-intervals indexes intervals) tones))

(tones-on-indexes-with-intervals
 [0 3 7]
 ["1" "b3" "5"]
 all-tones)



(defn rotate-matrix
  "Rotate matrix.
  In:  `[[1 2 3] [3 4 5] [6 7 8]]`
  Out: `[[1 3 6] [2 4 7] [3 5 8]]`"
  [matrix]
  (if-not (seq matrix)
    matrix
    (->> matrix
       (apply mapv vector)
       (mapv identity))))

(rotate-matrix
 [["3" nil nil]
  [nil "1" nil]
  ["5" nil nil]
  [nil nil nil]
  [nil nil nil]
  [nil nil nil]])



(defn trim-matrix
  ([fretboard-matrix]
   (trim-matrix (partial every? nil?) fretboard-matrix))
  ([pred fretboard-matrix]
   (if (empty? fretboard-matrix)
     fretboard-matrix
     (let [rotated (rotate-matrix fretboard-matrix)
           first?  (->> rotated first pred)
           last?   (->> rotated last pred)]
     (cond
       (and first? last?)
       (->> rotated
            (drop 1)
            (drop-last 1)
            (rotate-matrix)
            (trim-matrix pred))
       (and first? (not last?))
       (->> rotated
            (drop 1)
            (rotate-matrix)
            (trim-matrix pred))
       (and (not first?) last?)
       (->> rotated
            (drop-last 1)
            (rotate-matrix)
            (trim-matrix pred))
       :else fretboard-matrix)))))

(trim-matrix
 [["3" nil nil]
  [nil "1" nil]
  ["5" nil nil]
  [nil nil nil]
  [nil nil nil]
  [nil nil nil]])

(trim-matrix
 [[nil "1" nil nil]
  [nil "5" nil nil]
  [nil "b3" nil nil]
  [nil nil nil "1"]
  [nil nil nil "5"]
  [nil "1" nil nil]])



(defn intevals-string->intervals-matrix
  [interval]
  (->> interval
       (str/split-lines)
       (mapv (fn [line]
               (->> line
                    str/trim
                    (re-seq #"(b{0,2}#{0,2}\d{1,2})|-")
                    (mapv (comp #(when-not (= "-" %) %) first)))))))

(intevals-string->intervals-matrix
  "    3   -   -
   -   bb1   -
   5   -   -
   -   -   -
   -   -   -
   -   -   -")



(defn find-fretboard-pattern
  "Find patterns on the "
  [all-tones intervals-map-by-function key-of interval-matrix fretboard-matrix]
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
                                      (let [interval-semitones (get-in intervals-map-by-function [interval' :semitones])
                                            fretboard-tone     (nth
                                                                (rotate-until #(% key-of) all-tones)
                                                                interval-semitones)]
                                        (and
                                         (= (-> combinations-p first count)
                                            interval-matrix-width)
                                         (= tone fretboard-tone))))))
            pattern-check
            (->> combinations
                 (remove (comp nil? first))
                 (map (fn [[interval' {:keys [tone] :as tone'}]]
                        (let [interval-semitones (get-in intervals-map-by-function [interval' :semitones])
                              fretboard-tone     (nth
                                                  (rotate-until #(% key-of) all-tones)
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
 intervals/intervals-map-by-function
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



(defn find-chord [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (rotate-until #(% root-tone) all-tones)]
    (->> chords-map
         (vals)
         (filter (fn [{:chord/keys [intervals indexes]}]
                   (let [chord-to-serch ((juxt-indexes-and-intervals indexes intervals) tones)
                         chord-to-match chord-tones]
                     (and (= (count chord-to-serch) (count chord-to-match))
                          (->> (map
                                #(= %1 %2)
                                chord-to-serch
                                chord-to-match)
                               (every? true?))))))
         (first))))


(find-chord
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
 [:c :eb :g])



(defn chord-name
  [all-tones chords-map chord-tones]
  (let [root-tone             (first chord-tones)
        {:chord/keys [sufix]} (find-chord chords-map all-tones chord-tones)]
    (str (-> root-tone name str/lower-case str/capitalize) sufix)))

(chord-name
 all-tones
 {:major
  #:chord{:id           :major,
          :intervals    ["1" "3" "5"],
          :indexes      [0 4 7],
          :title        "major",
          :order        1,
          :sufix        "",
          :explanation  "major",
          :display-text "major"}}

 [:c :e :g])



(defn map-matrix
  "Flatten matrix into one dim list and run `f` on each item. The convert it back
  into a matrix."
  [f matrix]
  (->> (apply concat matrix)
       (map f)
       (partition (-> matrix first count))
       (mapv #(mapv identity %))))

(map-matrix
 inc
 [[1]
  [2]
  [3]])



(defn intervals->tones
  "Match intervals in a key with tones.

  "
  [all-tones intervals-map-by-function key-of intervals]
  (let [rotated-tones (rotate-until #(% key-of) all-tones)]
    (->> intervals
         (mapv
          (fn [interval]
            (let [interval-index (get-in intervals-map-by-function [interval :semitones])]
              (sharp-or-flat
               (nth rotated-tones interval-index)
               interval)))))))

(intervals->tones
 all-tones
 v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
 :c
 ["1" "b3" "5"])



(defn add-layer [f fretboard-matrix]
  (map-matrix #(assoc % :out (f %)) fretboard-matrix))

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


(->> (find-fretboard-pattern
      all-tones
      intervals/intervals-map-by-function
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
     (add-layer
      #_add-flats
      #_add-sharps
      add-pattern))

(->> (fretboard-strings
       all-tones
       [:e :b :g :d :a :e]
       4)
     (add-layer
      (partial add-chord-tones [:e :b :g])
      #_(partial add-intervals [[:e "1"] [:b "b3"] [:g "5"]])))


(defn fretboard-str
  [tone-f matrix]
  (let [add-table-stuff
        (fn [row]
          (str "|" (apply str (interpose "|" (map #(fformat " %-3s" %) row))) "|"))
        rows
        (->> matrix
             (map
              (fn [row]
                (->> row
                     (map tone-f))))
             (map add-table-stuff))
        row-length (-> rows first count)]
    (->> rows
         (list-insert (add-table-stuff (->> matrix first (map :x))) 0)
         (list-insert (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
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
      intervals/intervals-map-by-function
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
      add-pattern)
     ;; (trim-matrix #(every? nil? (map :out %))) ;; Trim fretboard
     (fretboard-str (fn [{:keys [out]}] (if (nil? out) "" out)))
     (println))


(->> (find-fretboard-pattern
      all-tones
      intervals/intervals-map-by-function
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



;; REPL stuff below line




(let [tones     [:d :f# :a]
      tones-set (set tones)]
  (->> utils-v2/all-tones
       (utils-v2/rotate-until #(% :d#))
       (map (fn [tone]
              (cond-> {:tone tone}
                (seq (set/intersection tones-set tone))
                (assoc :match true))))))

(set/intersection #{:d :f# :a} #{:gb :f#})
(set/intersection #{:d :f# :a} #{:gb})













;; Find the chord, cartesian product of all chords and key-of
(->> (for [{:chord/keys [intervals] :as chord} (vals @v2.se.jherrlin.music-theory.definitions/chords-atom)
           tone                                (apply concat all-tones)]
       (let [tones (intervals->tones
                    all-tones
                    sharp-or-flat
                    rotate-until
                    v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
                    tone
                    intervals)]
         (assoc chord :key-of tone :tones (set tones))))
     (group-by :tones)
     ;; (map (fn [[a b]] [a (count b)]))
     ;; (sort-by  second #(compare %2 %1))
     )



(->> (for [{:chord/keys [intervals indexes] :as chord} (vals @v2.se.jherrlin.music-theory.definitions/chords-atom)
           tone                                        (apply concat all-tones)]
       (assoc chord :derp (->> ((juxt-indexes indexes)
                                (rotate-until #(% tone) all-tones))
                               (apply concat)
                               (set)))))
