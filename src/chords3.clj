(ns chords3
  (:require [clojure.string :as str]
            [utils :refer [find-root fret-table-with-tones juxt-intervals find-chord succ]]))

(comment
  (remove-ns 'chords3)
  )

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

;; --------------------
;; Intervals
;; --------------------
(def perfect-unison     0)
(def root               perfect-unison)
(def minor-second       1)
(def major-second       2)
(def minor-third        3)
(def major-third        4)
(def perfect-fourth     5)
(def augmented-fourth   6)
(def diminished-fifth   6)
(def perfect-fifth      7)
(def minor-sixth        8)
(def major-sixth        9)
(def diminished-seventh 9)
(def minor-seventh      10)
(def major-seventh      11)
(def perfect-octave     perfect-unison)
;; --------------------
;; Intervals
;; --------------------

;; --------------------
;; Scales
;; --------------------
(def major-scale-tones
  "Major"
  (juxt-intervals
   [root major-second major-third perfect-fourth perfect-fifth major-sixth major-seventh]))

(major-scale-tones tones)

(def minor-scale-tones
  "Minor"
  (juxt-intervals
   [root minor-second minor-third perfect-fourth perfect-fifth minor-sixth minor-seventh]))

(minor-scale-tones tones)

(def minor-pentatonic-scale-tones
  "Minor pentatonic"
  (juxt-intervals
   [root minor-third perfect-fourth perfect-fifth minor-seventh]))

(def major-pentatonic-scale-tones
  "Major pentatonic"
  (juxt-intervals
   [root major-second major-third perfect-fifth major-sixth]))

(def minor-pentatonic-blues-scale-tones
  "Minor pentatonic blues"
  (juxt-intervals
   [root minor-third perfect-fourth diminished-fifth perfect-fifth minor-seventh]))

(minor-pentatonic-blues-scale-tones (find-root :a tones))

(def scales-map
  {:minor-pentatonic-blues
   {:type :scale,
    :id   :minor-pentatonic-blues,
    :f    minor-pentatonic-blues-scale-tones,
    :s    "Minor pentatonic blues"},
   :major-pentatonic
   {:type :scale,
    :id   :major-pentatonic,
    :f    major-pentatonic-scale-tones,
    :s    "Major pentatonic"},
   :major {:type :scale, :id :major, :f major-scale-tones, :s "Major"},
   :minor {:type :scale, :id :minor, :f minor-scale-tones, :s "Minor"},
   :minor-pentatonic
   {:type :scale,
    :id   :minor-pentatonic,
    :f    minor-pentatonic-scale-tones,
    :s    "Minor pentatonic"}})

(comment
  (->> (ns-publics 'chords3)
       (map (fn [[k v]]
              {:f   (symbol k)
               :kw  (-> (str k)
                        (str/replace "-scale-tones" "")
                        (keyword))
               :s   (str k)
               :doc (:doc (meta v))}))
       (filter (comp #(str/includes? % "-scale-tones") :s))
       (map (fn [{:keys [f doc kw]}]
              [kw {:type :scale
                   :id   kw
                   :f    f
                   :s    doc}]))
       (into {}))
  )
;; --------------------
;; Scales
;; --------------------

;; --------------------
;; Chords
;; --------------------
(def major-chord-tones
  ""
  (juxt-intervals
   [root major-third perfect-fifth]))

(major-chord-tones tones) ;; => [:c :e :g]

(def minor-chord-tones
  "m"
  (juxt-intervals
   [root minor-third perfect-fifth]))

(minor-chord-tones tones) ;; => [:c :d# :g]

(def sus2-chord-tones
  "sus2"
  (juxt-intervals
   [root major-second perfect-fifth]))

(sus2-chord-tones tones) ;; => [:c :d :g]

(def sus4-chord-tones
  "sus4"
  (juxt-intervals
   [root perfect-fourth perfect-fifth]))

(sus4-chord-tones tones)  ;; => [:c :f :g]

(def major-seven-chord-tones
  "7"
  (juxt-intervals
   [root major-third perfect-fifth minor-seventh]))

(major-seven-chord-tones tones) ;; => [:c :e :g :a#]

(def minor-seven-chord-tones
  "m7"
  (juxt-intervals
   [root minor-third perfect-fifth minor-seventh]))

(minor-seven-chord-tones tones)  ;; => [:c :d# :g :a#]

(def minor-maj-seven-chord-tones
  "m(maj7)"
  (juxt-intervals
   [root minor-third perfect-fifth major-seventh]))

(minor-maj-seven-chord-tones tones) ;; => [:c :d# :g :b]

(def major-maj-seven-chord-tones
  "maj7"
  (juxt-intervals
   [root major-third perfect-fifth major-seventh]))

(major-maj-seven-chord-tones tones) ;; => [:c :e :g :b]

(def minor-seven-flat-5-chord-tones
  "m7b5"
  (juxt-intervals
   [root minor-third diminished-fifth minor-seventh]))

(minor-seven-flat-5-chord-tones tones) ;; => [:c :d# :f# :a#]

(def major-seven-flat-5-chord-tones
  "(maj7)b5"
  (juxt-intervals
   [root major-third diminished-fifth minor-seventh]))

(major-seven-flat-5-chord-tones tones)  ;; => [:c :e :f# :a#]

(def fifth-chord-tones
  "5"
  (juxt-intervals
   [root perfect-fifth]))

(def diminished-triad-chord-tones
  "dim"
  (juxt-intervals
   [root minor-third diminished-fifth]))

(def diminished-seventh-chord-tones
  "dim7"
  (juxt-intervals
   [root minor-third diminished-fifth diminished-seventh]))

(diminished-seventh-chord-tones tones)

(def chords-map
  {:diminished-seventh
   {:type :chord,
    :id   :diminished-seventh,
    :f    diminished-seventh-chord-tones,
    :s    "dim7"},
   :fifth {:type :chord, :id :fifth, :f fifth-chord-tones, :s "5"},
   :minor-seven
   {:type :chord, :id :minor-seven, :f minor-seven-chord-tones, :s "m7"},
   :major {:type :chord, :id :major, :f major-chord-tones, :s ""},
   :major-seven
   {:type :chord, :id :major-seven, :f major-seven-chord-tones, :s "7"},
   :minor-seven-flat-5
   {:type :chord,
    :id   :minor-seven-flat-5,
    :f    minor-seven-flat-5-chord-tones,
    :s    "m7b5"},
   :diminished-triad
   {:type :chord,
    :id   :diminished-triad,
    :f    diminished-triad-chord-tones,
    :s    "dim"},
   :major-maj-seven
   {:type :chord,
    :id   :major-maj-seven,
    :f    major-maj-seven-chord-tones,
    :s    "maj7"},
   :major-seven-flat-5
   {:type :chord,
    :id   :major-seven-flat-5,
    :f    major-seven-flat-5-chord-tones,
    :s    "(maj7)b5"},
   :sus2  {:type :chord, :id :sus2, :f sus2-chord-tones, :s "sus2"},
   :minor {:type :chord, :id :minor, :f minor-chord-tones, :s "m"},
   :sus4  {:type :chord, :id :sus4, :f sus4-chord-tones, :s "sus4"},
   :minor-maj-seven
   {:type :chord,
    :id   :minor-maj-seven,
    :f    minor-maj-seven-chord-tones,
    :s    "m(maj7)"}})

(comment
  (->> (ns-publics 'chords3)
       (map (fn [[k v]]
              {:f   (symbol k)
               :kw  (-> (str k)
                        (str/replace "-chord-tones" "")
                        (keyword))
               :doc (:doc (meta v))
               :s   (str k)}))
       (filter (comp #(str/includes? % "chord-tones") :s))
       (map (fn [{:keys [f doc kw]}]
              [kw {:type :chord
                   :id   kw
                   :f    f
                   :s    doc}]))
       (into {}))
  )

(find-chord chords-map tones [:c :g :e]) ;; => "C"
(find-chord chords-map tones [:b :d :f]) ;; => "Bdim"
(find-chord chords-map tones [:e :g :b]) ;; => "Em"

;; --------------------
;; Chords
;; --------------------

;; --------------------
;; Harmonization
;; --------------------

(rest [:a :b :c :d])

(major-scale-tones tones)  ;; => [:c :d :e :f :g :a :b]

(def triad   (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def seventh (juxt #(nth % 0) #(nth % 2) #(nth % 4) #(nth % 6)))


(find-root :d (major-scale-tones tones))
(find-root :d tones)

(let [tone        :c
      all-tones   tones
      scale       major-scale-tones
      scale-tones (scale (find-root tone all-tones))
      f           seventh]
  (loop [counter       0
         [this & rest] scale-tones
         chords        []]
    (if (= counter 7)
      chords
      (let [chord-tones (f (find-root this scale-tones))
            chord-name  (find-chord chords-map all-tones chord-tones)]
        (recur
         (inc counter)
         rest
         (conj chords {:chord-name  chord-name
                       :chord-tones chord-tones})
         )))))

;; --------------------
;; Harmonization
;; --------------------

(print
 (fret-table-with-tones tones [:c :e :g :b]))
