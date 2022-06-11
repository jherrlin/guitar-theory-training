(ns chords3
  (:require [clojure.string :as str]
            [utils :refer [docstring->m find-chord find-root fret-table-with-tones juxt-intervals]]))

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

(def intervals-map
  {:perfect-unison     {:f        perfect-unison
                        :doc/full "perfect unison"}
   :root               {:f        root
                        :doc/full "root"}
   :minor-second       {:f        minor-second
                        :doc/full "minor second"}
   :major-second       {:f        major-second
                        :doc/full "major second"}
   :minor-third        {:f        minor-third
                        :doc/full "minor third"}
   :major-third        {:f        major-third
                        :doc/full "major third"}
   :perfect-fourth     {:f        perfect-fourth
                        :doc/full "perfect fourth"}
   :augmented-fourth   {:f        augmented-fourth
                        :doc/full "augmented fourth"}
   :diminished-fifth   {:f        diminished-fifth
                        :doc/full "diminished fifth"}
   :perfect-fifth      {:f        perfect-fifth
                        :doc/full "perfect fifth"}
   :minor-sixth        {:f        minor-sixth
                        :doc/full "minor sixth"}
   :major-sixth        {:f        major-sixth
                        :doc/full "major sixth"}
   :diminished-seventh {:f        diminished-seventh
                        :doc/full "diminished seventh"}
   :minor-seventh      {:f        minor-seventh
                        :doc/full "minor seventh"}
   :major-seventh      {:f        major-seventh
                        :doc/full "major seventh"}
   :perfect-octave     {:f        perfect-octave
                        :doc/full "perfect octave"}})

(defn interval [tones tone i]
  (nth (find-root tone tones) i))

(def interval-p (partial interval tones))

(interval-p :c perfect-fifth)  ;; => :g
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
  "
  short:
  full:      major
  functions: 1 3 5"
  (juxt-intervals
   [root major-third perfect-fifth]))

(major-chord-tones tones) ;; => [:c :e :g]

(def minor-chord-tones
  "
  short:     m
  full:      minor
  functions: 1 b3 5"
  (juxt-intervals
   [root minor-third perfect-fifth]))

(minor-chord-tones tones) ;; => [:c :d# :g]

(def sus2-chord-tones
  "
  short:     sus2
  full:      suspended 2
  functions: 1 2 5"
  (juxt-intervals
   [root major-second perfect-fifth]))

(sus2-chord-tones tones) ;; => [:c :d :g]

(def sus4-chord-tones
  "
  short:     sus2
  full:      suspended 4
  functions: 1 4 5"
  (juxt-intervals
   [root perfect-fourth perfect-fifth]))

(sus4-chord-tones tones)  ;; => [:c :f :g]

(def major-seven-chord-tones
  "
  short:     7
  full:      major seven
  functions: 1 3 5 b7"
  (juxt-intervals
   [root major-third perfect-fifth minor-seventh]))

(major-seven-chord-tones tones) ;; => [:c :e :g :a#]

(def minor-seven-chord-tones
  "
  short:     m7
  full:      minor seven
  functions: 1 b3 5 b7"
  (juxt-intervals
   [root minor-third perfect-fifth minor-seventh]))

(minor-seven-chord-tones tones)  ;; => [:c :d# :g :a#]

(def minor-maj-seven-chord-tones
  "
  short:     m(maj7)
  full:      minor major seven
  functions: 1 b3 5 7"
  (juxt-intervals
   [root minor-third perfect-fifth major-seventh]))

(minor-maj-seven-chord-tones tones) ;; => [:c :d# :g :b]

(def major-maj-seven-chord-tones
  "maj7
  short:     maj7
  full:      major major seven
  functions: 1 3 5 7"
  (juxt-intervals
   [root major-third perfect-fifth major-seventh]))

(major-maj-seven-chord-tones tones) ;; => [:c :e :g :b]

(def minor-seven-flat-5-chord-tones
  "
  short:     m7b5
  full:      minor sevent flat 5
  functions: 1 b3 d5 b7"
  (juxt-intervals
   [root minor-third diminished-fifth minor-seventh]))

(minor-seven-flat-5-chord-tones tones) ;; => [:c :d# :f# :a#]

(def major-seven-flat-5-chord-tones
  "
  short:     (maj7)b5
  full:      major major sevent flat 5
  functions: 1 b d5 7"
  (juxt-intervals
   [root major-third diminished-fifth major-seventh]))

(major-seven-flat-5-chord-tones tones)

(def fifth-chord-tones
  "
  short:     5
  full:      Power chord
  functions: 1 5"
  (juxt-intervals
   [root perfect-fifth]))

(def diminished-triad-chord-tones
  "
  short:     dim
  full:      diminished fifth
  functions: 1 b3 d5"
  (juxt-intervals
   [root minor-third diminished-fifth]))

(def diminished-seventh-chord-tones
  "dim7
  short:     dim7
  full:      diminished seventh
  functions: 1 b3 d5 b7"
  (juxt-intervals
   [root minor-third diminished-fifth diminished-seventh]))

(diminished-seventh-chord-tones tones)

(def chords-map
  {:diminished-seventh
   {:f             diminished-seventh-chord-tones,
    :id            :diminished-seventh,
    :doc/short     "dim7",
    :doc/full      "diminished seventh",
    :doc/functions "1 b3 d5 b7",
    :s             "diminished-seventh-chord-tones"},
   :fifth
   {:f             fifth-chord-tones,
    :id            :fifth,
    :doc/short     "5",
    :doc/full      "Power chord",
    :doc/functions "1 5",
    :s             "fifth-chord-tones"},
   :minor-seven
   {:f             minor-seven-chord-tones,
    :id            :minor-seven,
    :doc/short     "m7",
    :doc/full      "minor seven",
    :doc/functions "1 b3 5 b7",
    :s             "minor-seven-chord-tones"},
   :major
   {:f             major-chord-tones,
    :id            :major,
    :doc/short     "",
    :doc/full      "major",
    :doc/functions "1 3 5",
    :s             "major-chord-tones"},
   :major-seven
   {:f             major-seven-chord-tones,
    :id            :major-seven,
    :doc/short     "7",
    :doc/full      "major seven",
    :doc/functions "1 3 5 b7",
    :s             "major-seven-chord-tones"},
   :minor-seven-flat-5
   {:f             minor-seven-flat-5-chord-tones,
    :id            :minor-seven-flat-5,
    :doc/short     "m7b5",
    :doc/full      "minor sevent flat 5",
    :doc/functions "1 b3 d5 b7",
    :s             "minor-seven-flat-5-chord-tones"},
   :diminished-triad
   {:f             diminished-triad-chord-tones,
    :id            :diminished-triad,
    :doc/short     "dim",
    :doc/full      "diminished fifth",
    :doc/functions "1 b3 d5",
    :s             "diminished-triad-chord-tones"},
   :major-maj-seven
   {:f             major-maj-seven-chord-tones,
    :id            :major-maj-seven,
    :doc/short     "maj7",
    :doc/full      "major major seven",
    :doc/functions "1 3 5 7",
    :s             "major-maj-seven-chord-tones"},
   :major-seven-flat-5
   {:f             major-seven-flat-5-chord-tones,
    :id            :major-seven-flat-5,
    :doc/short     "(maj7)b5",
    :doc/full      "major major sevent flat 5",
    :doc/functions "1 b d5 7",
    :s             "major-seven-flat-5-chord-tones"},
   :sus2
   {:f             sus2-chord-tones,
    :id            :sus2,
    :doc/short     "sus2",
    :doc/full      "suspended 2",
    :doc/functions "1 2 5",
    :s             "sus2-chord-tones"},
   :minor
   {:f             minor-chord-tones,
    :id            :minor,
    :doc/short     "m",
    :doc/full      "minor",
    :doc/functions "1 b3 5",
    :s             "minor-chord-tones"},
   :sus4
   {:f             sus4-chord-tones,
    :id            :sus4,
    :doc/short     "sus2",
    :doc/full      "suspended 4",
    :doc/functions "1 4 5",
    :s             "sus4-chord-tones"},
   :minor-maj-seven
   {:f             minor-maj-seven-chord-tones,
    :id            :minor-maj-seven,
    :doc/short     "m(maj7)",
    :doc/full      "minor major seven",
    :doc/functions "1 b3 5 7",
    :s             "minor-maj-seven-chord-tones"}})

(def find-chord-p (partial find-chord chords-map tones))

(comment
  (->> (ns-publics 'chords3)
       (map (fn [[k v]]
              (let [docstring                      (:doc (meta v))
                    {:keys [short full functions]} (docstring->m docstring)
                    kw                             (-> (str k)
                                                       (str/replace "-chord-tones" "")
                                                       (keyword))]
                {:f             (symbol k)
                 :id            kw
                 :doc/short     short
                 :doc/full      full
                 :doc/functions functions
                 :s             (str k)})))
       (filter (comp #(str/includes? % "chord-tones") :s))
       (map (juxt :id identity))
       (into {}))
  )

(find-chord chords-map tones [:c :g :e]) ;; => "C"
(find-chord chords-map tones [:b :d :f]) ;; => "Bdim"
(find-chord chords-map tones [:e :g :b]) ;; => "Em"

(find-chord-p [:e :g :b])

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

(def harmonizations-map
  {:triad   triad
   :seventh seventh})

(find-root :d (major-scale-tones tones))
(find-root :d tones)

(defn harmonizations [scales-map chords-map all-tones tone scale f]
  (let [scale-tones ((get-in scales-map [scale :f]) (find-root tone all-tones))]
    (->> scale-tones
         (reduce
          (fn [m t]
            (let [chord-tones (f (find-root t scale-tones))
                  chord-name  (find-chord chords-map all-tones chord-tones)]
              (conj m {:root        tone
                       :scale       scale
                       :chord-name  chord-name
                       :chord-tones chord-tones})))
          [])
         (mapv
          (fn [i m]
            (assoc m :position i))
          ["I" "ii" "iii" "IV" "V" "vi" "vii"]))))

(def harmonizations-p (partial harmonizations scales-map chords-map tones))

(harmonizations-p :c :major triad)
(harmonizations-p :c :minor triad)
(harmonizations-p :c :minor seventh)

(->> (for [tone tones
           mm   [:major :minor]
           f    [:triad :seventh]]
       {:tone tone
        :m mm
        :t f
        :harmonizations
        (harmonizations-p tone mm (get harmonizations-map f))})
     #_(apply concat))

;; --------------------
;; Harmonization
;; --------------------

;; --------------------
;; Org-drill
;; --------------------

(def fret-table-with-tones-p (partial fret-table-with-tones tones))

(defn tones-str [tones]
  (->> tones (map (comp str/upper-case name)) (str/join ", ")))

(spit
 "/tmp/music-theory-drills.org"
 (with-out-str
   (print
    (str
     "#+STARTUP: overview\n\n"
     (->> (for [tone          tones
                [_ {:keys [f]
                    s     :doc/short
                    :as   m}] chords-map]
            (let [root-str    (-> tone name str/upper-case)
                  chord-tones (f (find-root  tone tones))]
              (merge
               m
               {:chord-name      (str root-str s)
                :chord-tones     chord-tones
                :chord-tones-str (tones-str chord-tones)})))
          (map (fn [{:keys     [chord-name chord-tones chord-tones-str]
                     :doc/keys [full functions]}]
                 (str
                  (str
                   "** " (format "%-60s:music:theory:chords:drill:" (str "Notes in " chord-name))
                   "\n\n"
                   "   What tones are in " chord-name " chord?"
                   "\n\n"
                   "*** Answer \n\n    " chord-tones-str
                   "\n\n"
                   "    " "Functions:   " functions
                   "\n"
                   "    " "Description: " (-> chord-tones first name str/upper-case) " " full
                   "\n\n\n"
                   (fret-table-with-tones-p chord-tones)
                   "\n")
                  (str
                   "** " (format "%-60s:music:theory:chords:drill:" (str "Name the chord: " chord-tones-str))
                   "\n\n"
                   "   Name the chord with the following tones: " chord-tones-str
                   "\n\n"
                   "*** Answer "
                   "\n\n"
                   "    " chord-name
                   "\n\n"
                   "    " "Functions:   " functions
                   "\n"
                   "    " "Description: " (-> chord-tones first name str/upper-case) " " full
                   "\n\n\n"
                   (fret-table-with-tones-p chord-tones)
                   "\n"))))
          (apply str))
     (->>
      (for [tone                  tones
            [_ {:keys [f]
                full  :doc/full}] intervals-map]
        (let [interval-tone (interval-p tone f)]
          {:interval          (str/capitalize full)
           :nr-of-semitones   f
           :tone              tone
           :interval-tone     interval-tone
           :tone-str          (-> tone name str/upper-case)
           :interval-tone-str (-> interval-tone name str/upper-case)}))
      (map (fn [{:keys [interval interval-tone interval-tone-str nr-of-semitones tone tone-str]}]
             (str
              (str
               "** " (format "%-60s:music:theory:intervals:drill:" (str interval " from " tone-str))
               "\n\n"
               "   " "What is the tone in " interval " interval from tone " tone-str "?"
               "\n"
               "   " "How many semitones are in the interval?"
               "\n\n"
               "*** Answer \n\n"
               "    " "Tone in interval:  " interval-tone-str
               "\n"
               "    " "Numer of semitones: " nr-of-semitones
               "\n\n"
               "\n\n"
               (fret-table-with-tones-p [tone interval-tone])
               "\n\n")

              (str
               "** " (format "%-60s:music:theory:intervals:drill:" (str "Interval " nr-of-semitones " semitones from " tone-str))
               "\n\n"
               "   " "Name the interval " nr-of-semitones " semitones from " tone-str " and the tone in the interval."
               "\n\n"
               "*** Answer \n\n"
               "    " interval-tone-str
               "\n\n"
               (fret-table-with-tones-p [tone interval-tone])
               "\n\n"))))
      (apply str))
     (->> (for [tone              tones
           [_ {:keys [s f]}] scales-map]
       (let [scale-tones (f (find-root tone tones))
             tones-str   (->> scale-tones (map (comp str/upper-case name)) (str/join ", "))]
         {:scale-tones scale-tones
          :display     s
          :tone        (-> tone name str/upper-case)
          :tones-str   tones-str}))
     (map (fn [{:keys [scale-tones display tone tones-str]}]
            (str
             "** " (format "%-60s:music:theory:scales:drill:" (str "Tones in " tone " " display " scale"))
             "\n\n"
             "   " "What tones are in the " tone " " display " scale?"
             "\n\n"
             "*** Answer \n\n"
             "    " "The tones are: " tones-str
             "\n\n\n"
             (fret-table-with-tones-p scale-tones)
             "\n")))
     (apply str))))))




;; --------------------
;; Org-drill
;; --------------------


(print
 (fret-table-with-tones tones [:c :e :g :b]))
