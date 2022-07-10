(ns se.jherrlin.music-theory1
  (:require [clojure.string :as str]))


(def tones1
  [#{:c}
   #{:db :c#}
   #{:d}
   #{:d# :eb}
   #{:e}
   #{:f}
   #{:gb :f#}
   #{:g}
   #{:g# :ab}
   #{:a}
   #{:bb :a#}
   #{:b}])


;; Utils
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
   tones1)

(defn sharp-or-flat [tone interval]
  {:pre [(set? tone)]}
  (cond
    (= 1 (count tone))           (first tone)
    (str/includes? interval "b") (first (filter (comp #(str/includes? % "b") name) tone))
    (str/includes? interval "#") (first (filter (comp #(str/includes? % "#") name) tone))))

(sharp-or-flat
   #{:g#}
   "3#")

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
   tones1
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
 tones1
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

(intervals->tones
 tones1
 sharp-or-flat
 rotate-until
 se.jherrlin.music-theory.intervals/intervals-map-by-function
 :c
 ["1" "b3" "b5"]
 )

(defn find-pattern [all-tones intervals-map-by-function fretboard key-of interval-matrix]
  (let [interval-matrix-width     (-> interval-matrix first count)
        fretboard-count           (-> fretboard first count)]
    (loop [counter           0
           found-pattern-xys #{}]
      (let [combinations
            (->>  fretboard
                  (mapv (comp vec (partial take interval-matrix-width) (partial drop counter)))
                  (apply concat)
                  (mapv vector (apply concat interval-matrix)))
            pattern-check
            (->> combinations
                 (remove (comp nil? first))
                 (map (fn [[interval' {:keys [tone] :as tone'}]]
                        (let [interval-semitones (get-in intervals-map-by-function [interval' :semitones])
                              fretboard-tone     (nth
                                                  (rotate-until #(% key-of) all-tones)
                                                  interval-semitones)]
                          (assoc tone' :match? (= tone fretboard-tone))))))
            found-pattern-xys'
            (->> pattern-check
                 (filter :match?)
                 (map (juxt :x :y))
                 (set)
                 (into found-pattern-xys))]
        (if-not (= fretboard-count counter)
          (recur
           (inc counter)
           found-pattern-xys')
          (->> fretboard
               (apply concat)
               (map (fn [{:keys [x y] :as m}]
                      (assoc m :pattern-match? (contains? found-pattern-xys' [x y]))))
               (partition fretboard-count)
               (mapv #(mapv identity %))))))))

(find-pattern
 tones1
 se.jherrlin.music-theory.intervals/intervals-map-by-function
 (fretboard-strings
  rotate-until
  tones1
  [:e :b :g :d :a :e]
  25)
 :g
 [[nil nil nil nil]
  [nil nil nil nil]
  [nil nil nil nil]
  ["6" nil "7" "1"]
  ["3" "4" nil "5"]
  [nil "1" nil "2"]])
