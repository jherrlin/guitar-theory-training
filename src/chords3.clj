(ns chords3
  (:require [clojure.string :as str]))

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

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

(defn find-root
  [tone tones]
  {:pre [((set tones) tone)]}
  (->> (cycle tones)
       (drop-while #(not= % tone))
       (take 12)
       (vec)))

(defn juxt-intervals [intervals]
  (apply juxt
         (map
          (fn [interval] (fn [tones] (nth tones interval)))
          intervals)))

;; Scales
(def major-scale-tones
  (juxt-intervals
   [root major-second major-third perfect-fourth perfect-fifth major-sixth major-seventh]))

(major-scale-tones tones)

(def minor-scale-tones
  (juxt-intervals
   [root minor-second minor-third perfect-fourth perfect-fifth minor-sixth minor-seventh]))

(minor-scale-tones tones)

(def minor-pentatonic-scale-tones
  (juxt-intervals
   [root minor-third perfect-fourth perfect-fifth minor-seventh]))

(def major-pentatonic-scale-tones
  (juxt-intervals
   [root major-second major-third perfect-fifth major-sixth]))

(def minor-pentatonic-blues-scale-tones
  (juxt-intervals
   [root minor-third perfect-fourth diminished-fifth perfect-fifth minor-seventh]))

(minor-pentatonic-blues-scale-tones (find-root :a tones))

;; Scales

;; Chords
(def major-chord-tones
  (juxt-intervals
   [root major-third perfect-fifth]))

(major-chord-tones tones) ;; => [:c :e :g]

(def minor-chord-tones
  (juxt-intervals
   [root minor-third perfect-fifth]))

(minor-chord-tones tones) ;; => [:c :d# :g]

(def sus2-chord-tones
  (juxt-intervals
   [root major-second perfect-fifth]))

(sus2-chord-tones tones) ;; => [:c :d :g]

(def sus4-chord-tones
  (juxt-intervals
   [root perfect-fourth perfect-fifth]))

(sus4-chord-tones tones)  ;; => [:c :f :g]

(def major-seven-chord-tones
  (juxt-intervals
   [root major-third perfect-fifth minor-seventh]))

(major-seven-chord-tones tones) ;; => [:c :e :g :a#]

(def minor-seven-chord-tones
  (juxt-intervals
   [root minor-third perfect-fifth minor-seventh]))

(minor-seven-chord-tones tones)  ;; => [:c :d# :g :a#]

(def minor-maj-seven-chord-tones
  (juxt-intervals
   [root minor-third perfect-fifth major-seventh]))

(minor-maj-seven-chord-tones tones) ;; => [:c :d# :g :b]

(def major-maj-seven-chord-tones
  (juxt-intervals
   [root major-third perfect-fifth major-seventh]))

(major-maj-seven-chord-tones tones) ;; => [:c :e :g :b]

(def minor-seven-flat-5-chord-tones
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
   [root minor-third diminished-fifth minor-seventh diminished-seventh]))

(def chords-map
  {:diminished-seventh
   {:id :diminished-seventh,
    :f  diminished-seventh-chord-tones,
    :s  "dim7"},
   :fifth
   {:id :fifth,
    :f  fifth-chord-tones,
    :s  "5"},
   :minor-seven
   {:id :minor-seven,
    :f  minor-seven-chord-tones,
    :s  nil},
   :major       {:id :major, :f major-chord-tones, :s nil},
   :major-seven {:id :major-seven, :f major-seven-chord-tones, :s nil},
   :minor-seven-flat-5
   {:id :minor-seven-flat-5, :f minor-seven-flat-5-chord-tones, :s nil},
   :diminished-triad
   {:id :diminished-triad, :f diminished-triad-chord-tones, :s "dim"},
   :major-maj-seven
   {:id :major-maj-seven, :f major-maj-seven-chord-tones, :s nil},
   :major-seven-flat-5
   {:id :major-seven-flat-5, :f major-seven-flat-5-chord-tones, :s "(maj7)b5"},
   :sus2        {:id :sus2, :f sus2-chord-tones, :s nil},
   :minor       {:id :minor, :f minor-chord-tones, :s nil},
   :sus4        {:id :sus4, :f sus4-chord-tones, :s nil},
   :minor-maj-seven
   {:id :minor-maj-seven, :f minor-maj-seven-chord-tones, :s nil}})

(comment
  (->> (ns-publics 'chords3)
       keys
       (map str)
       (filter #(str/includes? % "chord-tones"))
       sort
       (map (fn [x]
              (let [k (-> x
                          (str/replace "-chord-tones" "")
                          (keyword))]
                [k
                 {:f       (symbol x)
                  :id      k
                  :display ()
                  }])))
       (into {}))


  (->> (ns-publics 'chords3)
       (map (fn [[k v]]
              {:f   (symbol k)
               :kw  (-> (str k)
                        (str/replace "-chord-tones" "")
                        (keyword))
               :s   (str k)
               :doc (:doc (meta v))}))
       (filter (comp #(str/includes? % "chord-tones") :s))
       (map (fn [{:keys [f s doc kw]}]
              [kw {:id kw
                   :f f
                   :s doc}]))
       (into {}))
  )
;; Chords
