(ns v2.se.jherrlin.music-theory.utils
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])))


(comment
  (def all-tones [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])
  )

(def all-tones [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])

#?(:cljs
   (defn fformat
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args))
   :clj (def fformat format))

(defn list-insert [elem index lst]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))

(defn rotate-until
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

(defn sharp-or-flat [tone interval]
  {:pre [(set? tone)]}
  (-> (cond
        (= 1 (count tone))           tone
        (str/includes? interval "b") (filter (comp #(str/includes? % "b") name) tone)
        :else                        (filter (comp #(str/includes? % "#") name) tone))
      (first)))

(sharp-or-flat
 #{:g#}
 "3#")

(defn juxt-indexes [indexes]
  (apply juxt
         (map
          (fn [index]
            (fn [tones]
              (nth tones index)))
          indexes)))

(defn tones-on-indexes [indexes tones]
  ((juxt-indexes indexes) tones))

(comment
  ((juxt-indexes [0 3 7]) all-tones)
  (tones-on-indexes [0 3 7] all-tones)
  )

(defn juxt-indexes-and-intervals [indexes intervals]
  (apply juxt
         (map
          (fn [index interval]
            (fn [tones]
              (sharp-or-flat
               (nth tones index)
               interval)))
          indexes
          intervals)))

(defn tones-on-indexes-with-intervals [indexes intervals tones]
  ((juxt-indexes-and-intervals indexes intervals) tones))

(comment
  ((juxt-indexes-and-intervals [0 3 7] ["1" "b3" "5"]) all-tones)
  (tones-on-indexes-with-intervals [0 3 7] ["1" "b3" "5"] all-tones)
  )

(defn intevals-string->intervals-matrix [interval]
  (->> interval
       (str/trim)
       (str/split-lines)
       (map str/trim)
       (mapv (fn [line]
               (->> line
                    (re-seq #"(b{0,2}#{0,2}\d)|-")
                    (mapv (comp #(when-not (= "-" %) %) first)))))))

(defn tones-and-intervals [tones intervals]
  {:pre [(= (count tones) (count intervals))]}
  (map
   (fn [t i]
     (sharp-or-flat t i))
   tones
   intervals))

(defn define-chord
  ([intervals-map chord-id intervals-str]
   (define-chord intervals-map chord-id {} intervals-str))
  ([intervals-map chord-id meta-data intervals-str]
   (let [intervals (->> (re-seq #"b{0,2}#{0,2}\d" intervals-str)
                        (vec))
         indexes   (->> intervals
                        (mapv #(get-in intervals-map [% :semitones])))]
     (merge
      {:chord/id        chord-id
       :chord/intervals intervals
       :chord/indexes   indexes
       :chord/title     (-> chord-id
                            name
                            (str/replace "-" " "))
       :chord/order     999}
      (->> meta-data
           (map (fn [[k v]]
                  [(->> k name (str "chord/") keyword) v]))
           (into {}))))))

(defn define-chord-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [pattern'  (intevals-string->intervals-matrix pattern)
         meta-data (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "chord-pattern/") keyword) v]))
                        (into {}))]
     (merge
      meta-data
      {:chord/pattern       pattern'
       :chord/pattern-id    pattern-name
       :chord/pattern-title (name pattern-name)
       :chord/pattern-str   pattern}))))

(defn define-scale-pattern
  ([pattern-name pattern]
   (define-scale-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [pattern'  (intevals-string->intervals-matrix pattern)
         meta-data (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "scale-pattern/") keyword) v]))
                        (into {}))]
     (merge
      meta-data
      {:scale-pattern/pattern       pattern'
       :scale-pattern/id            pattern-name
       :scale-pattern/pattern-title (name pattern-name)
       :scale-pattern/pattern-str   pattern}))))

(->> #{"1" "b3" "4" "5" "b5"}
     (sort-by last))
;; => ("1" "b3" "4" "b5" "5")

(define-chord-pattern
  :major-1
  {:name :major}
  "
   3   -   -   -
   -   1   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(defn define-scale
  ([intervals-map scale-id intervals-str]
   (define-scale intervals-map scale-id {} intervals-str))
  ([intervals-map scale-id meta-data intervals-str]
   (let [intervals (->> (re-seq #"b{0,2}#{0,2}\d" intervals-str)
                        (vec))
         indexes   (->> intervals
                        (mapv #(get-in intervals-map [% :semitones])))]
     (merge
      {:scale/id        scale-id
       :scale/intervals intervals
       :scale/indexes   indexes
       :scale/title     (-> scale-id
                            name
                            (str/replace "-" " "))
       :scale/order     999}
      (->> meta-data
           (map (fn [[k v]]
                  [(->> k name (str "scale/") keyword) v]))
           (into {}))))))

(comment
  (define-scale
    v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
    :major
    "1, 2, 3, 4, 5, 6, 7")
  )

(defn define-mode
  ([pattern-name pattern]
   (define-mode pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [pattern'  (intevals-string->intervals-matrix pattern)
         meta-data (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "mode/") keyword) v]))
                        (into {}))]
     (merge
      meta-data
      {:mode/pattern       pattern'
       :mode/id            pattern-name
       :mode/pattern-title (name pattern-name)
       :mode/pattern-str   pattern}))))

(define-mode :ionian
  {:scale :ionian}
  "
   -   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(defn define-triad-pattern
  ([pattern-name pattern]
   (define-mode pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [pattern'   (intevals-string->intervals-matrix pattern)
         meta-data  (->> meta-data
                         (map (fn [[k v]]
                                [(->> k name (str "triad-pattern/") keyword) v]))
                         (into {}))
         on-strings (->> pattern'
                         (map-indexed vector)
                         (vec)
                         (filter (fn [[string-idx intervals-on-string]]
                                   (some seq intervals-on-string)))
                         (map (fn [[string-idx _]] string-idx))
                         (vec))]
     (merge
      meta-data
      {:triad-pattern/on-strings    on-strings
       :triad-pattern/pattern       pattern'
       :triad-pattern/id            pattern-name
       :triad-pattern/pattern-title (name pattern-name)
       :triad-pattern/pattern-str   pattern}))))

(defn fretboard-string [rotate-until all-tones string-tune number-of-frets]
  (->> (mapv
        (fn [x t]
          {:x          x
           :tone       t})
        (iterate inc 0)
        (->> (rotate-until #(% string-tune) all-tones)
             (cycle)
             (take number-of-frets)))))

(fretboard-string
 rotate-until
 all-tones
 :g
 24)

(defn fretboard-strings
  [rotate-until all-tones string-tunes number-of-frets]
  (->> string-tunes
       (mapv
        (fn [y string-tune]
          (mapv
           #(assoc % :y y)
           (fretboard-string rotate-until all-tones string-tune number-of-frets)))
        (iterate inc 0))))

(fretboard-strings
 rotate-until
 all-tones
 [:e :b :g :d :a :e]
 12)

(defn intervals->tones
  [all-tones sharp-or-flat rotate-until intervals-map-by-function key-of intervals]
  (let [rotated-tones (rotate-until #(% key-of) all-tones)]
    (->> intervals
         (mapv
          (fn [interval]
            (let [interval-index (get-in intervals-map-by-function [interval :semitones])]
              (sharp-or-flat
               (nth rotated-tones interval-index)
               interval)))))))

(comment
  (intervals->tones
   all-tones
   sharp-or-flat
   rotate-until
   v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
   :c
   ["1" "b3" "5"])
  )

(defn find-pattern [all-tones intervals-map-by-function fretboard key-of interval-matrix]
  (let [interval-matrix-width (-> interval-matrix first count)
        fretboard-count       (-> fretboard first count)]
    (loop [counter           0
           found-pattern-xys #{}]
      (let [combinations-p
            (->>  fretboard
                  (mapv (comp vec (partial take interval-matrix-width) (partial drop counter))))
            combinations
            (->> combinations-p
                 (apply concat)
                 (mapv vector (apply concat interval-matrix)))
            box-match?     (->> combinations
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
          (->> fretboard
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

(comment
  (find-pattern
   all-tones
   se.jherrlin.music-theory.intervals/intervals-map-by-function
   (fretboard-strings
    rotate-until
    all-tones
    [:e :b :g :d :a :e]
    25)
   :c
   [[nil nil nil nil]
    [nil nil nil nil]
    [nil nil nil nil]
    ["6" nil "7" "1"]
    ["3" "4" nil "5"]
    [nil "1" nil "2"]]))

(defn match-chord-with-scales [scales-map chord-indexes]
  (->> scales-map
       (vals)
       (filter (fn [scale]
                 (let [scale-indexes (get scale :scale/indexes)]
                   (set/subset? (set chord-indexes) (set scale-indexes)))))))

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

(comment
  (find-chord
   #_@v2.se.jherrlin.music-theory.definitions/chords-atom
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
   [:c :e :g])
  )

(defn chord-name
  [chords-map all-tones chord-tones]
  (let [root-tone             (first chord-tones)
        {:chord/keys [sufix]} (find-chord chords-map all-tones chord-tones)]
    (str (-> root-tone name str/lower-case str/capitalize) sufix)))

(comment
  (chord-name
   ;; @v2.se.jherrlin.music-theory.definitions/chords-atom
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
   [:c :e :g])

  (let [tone :g
        {:chord/keys [intervals indexes] :as m}
        #:chord{:id        :minor,
                :intervals ["1" "b3" "5"],
                :indexes   [0 3 7],
                :title     "minor",
                :order     999}
        ;; (define-chord
        ;;   v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
        ;;   :minor
        ;;   "1 b3 5")
        ]
    (chord-name
     ;; @v2.se.jherrlin.music-theory.definitions/chords-atom
     {:minor
      #:chord{:id           :minor,
              :intervals    ["1" "b3" "5"],
              :indexes      [0 3 7],
              :title        "minor",
              :order        2,
              :sufix        "m",
              :explanation  "minor",
              :display-text "minor"}}
     all-tones
     (tones-on-indexes-with-intervals indexes intervals (rotate-until #(% tone) all-tones)))
    )
  )

(defn fretboard-str
  [matrix tone-f]
  (let [add-table-stuff
        (fn [row]
          (str "|" (apply str (interpose "|" (map #(fformat " %-3s" %) row))) "|"))
        rows
        (->> matrix
             (map
              (fn [row]
                (->> row
                     (map
                      (fn [{:keys [x y tone pattern-match? interval] :as m}]
                        (tone-f m))))))
             (map add-table-stuff))
        row-length (-> rows first count)]
    (->> rows
         (list-insert (add-table-stuff (->> matrix first count range (map str))) 0)
         (list-insert (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
         (str/join "\n"))))

(defn fretboard-tone-str-pattern-f [{:keys [x y tone pattern-match? interval] :as m}]
  (cond
    (nil? interval) ""
    (seq interval)  (-> (sharp-or-flat tone interval) name str/capitalize)))

(defn fretboard-tone-str-sharps-f [{:keys [x y tone pattern-match? interval] :as m}]
  (cond
    (nil? m) ""
    :else    (-> tone (sharp-or-flat "#") name str/capitalize)))

(defn fretboard-tone-str-flats-f [{:keys [x y tone pattern-match? interval] :as m}]
  (cond
    (nil? m) ""
    :else    (-> tone (sharp-or-flat "b") name str/capitalize)))

(defn fretboard-tone-str-chord-f [chord-tones {:keys [x y tone pattern-match? interval] :as m}]
  (if-let [tone' (first (set/intersection (set chord-tones) tone))]
    (-> tone' name str/capitalize)
    ""))

(defn fretboard-tone-str-chord-f-2
  [chord-tones-and-intervals
   {:keys [x y tone pattern-match? interval] :as m}]
  (if-let [i (->> chord-tones-and-intervals
                  (filter (fn [[tone' interval']]
                            (tone tone')))
                  (first)
                  (second))]
    i
    ""))

(defn fretboard-interval-f [{:keys [x y tone pattern-match? interval] :as m}]
  (if (nil? interval)
    ""
    interval))

(comment
  (print
   (fretboard-str
    (find-pattern
     all-tones
     v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
     (fretboard-strings
      rotate-until
      all-tones
      [:e :b :g :d :a :e]
      14)
     :c
     [["3" nil nil nil]
      [nil "1" nil nil]
      ["5" nil nil nil]
      [nil nil "3" nil]
      [nil nil nil "1"]
      [nil nil nil nil]])
    #_fretboard-tone-str-sharps-f
    #_fretboard-tone-str-flats-f
    fretboard-tone-str-pattern-f
    #_(partial fretboard-tone-str-chord-f [:c :e :g])
    ))


  (print
   (fretboard-str
    (find-pattern
     all-tones
     v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
     (fretboard-strings
      rotate-until
      all-tones
      [:e :b :g :d :a :e]
      16)
     :c
     [[nil nil nil]
      [nil "1" nil]
      ["5" nil nil]
      [nil nil "3"]
      [nil nil nil]
      [nil nil nil]])
    ;; fretboard-interval-f
    fretboard-tone-str-pattern-f))

  (println
   (fretboard-str
    (fretboard-strings
     rotate-until
     all-tones
     [:e :b :g :d :a :e]
     15)
    (partial
     fretboard-tone-str-chord-f [:c :e :g])))


;; Interval -> Tone
;;        1 -> C
;;        3 -> E
;;        5 -> G

  (print
   (fretboard-str
    (find-pattern
     all-tones
     v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
     (fretboard-strings
      rotate-until
      all-tones
      [:a :e :c :g]
      14)
     :c
     [[nil nil nil "1"]
      ["3" nil nil nil]
      ["1" nil nil nil]
      ["5" nil nil nil]])
    fretboard-interval-f))

  (println
   (fretboard-str
    (fretboard-strings
     rotate-until
     all-tones
     [:a :e :c :g]
     15)
    (partial
     fretboard-tone-str-chord-f [:c :e :g])))




  (find-pattern
   all-tones
   v2.se.jherrlin.music-theory.intervals/intervals-map-by-function
   (fretboard-strings
    rotate-until
    all-tones
    [:e :b :g :d :a :e]
    16)
   :c
   [[nil nil nil]
    [nil "1" nil]
    ["5" nil nil]
    [nil nil "3"]
    [nil nil nil]
    [nil nil nil]])
  )
