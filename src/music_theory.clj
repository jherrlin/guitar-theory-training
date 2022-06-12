(ns music-theory
  (:require [clojure.string :as str]
            [utils :refer [docstring->m find-chord find-root fret-table-with-tones juxt-intervals]]))

(comment
  (remove-ns 'music-theory)
  )

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

(def find-root-p #(find-root % tones))
(def fret-table-with-tones-p (partial fret-table-with-tones tones))

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

(comment
  (interval-p :c perfect-fifth)  ;; => :g
  )
;; --------------------
;; Intervals end
;; --------------------

;; --------------------
;; Scales
;; --------------------
(def major-scale-tones
  "Major
  functions: 1, 2, 3, 4, 5, 6, 7"
  (juxt-intervals
   [root major-second major-third perfect-fourth perfect-fifth major-sixth major-seventh]))

(def ionian-scale-tones
  "Ionian
  functions: 1, 2, 3, 4, 5, 6, 7"
  (juxt-intervals
   [root major-second major-third perfect-fourth perfect-fifth major-sixth major-seventh]))

(comment
  (major-scale-tones tones)            ;; => [:c :d :e :f :g :a :b]
  (major-scale-tones (find-root-p :e)) ;; => [:e :f# :g# :a :b :c# :d#]
  )

(def natural-minor-scale-tones
  "Natural minor
  functions: 1, 2, b3, 4, 5, b6, b7"
  (juxt-intervals
   [root major-second minor-third perfect-fourth perfect-fifth minor-sixth minor-seventh]))

(def minor-scale-tones
  "Natural minor
  functions: 1, 2, b3, 4, 5, b6, b7"
  natural-minor-scale-tones)

(def aeolian-scale-tones
  "Aeolian
  functions: 1, 2, b3, 4, 5, b6, b7"
  natural-minor-scale-tones)

(def harmonic-minor-scale-tones
  "Harmonic minor
  functions: 1, 2, b3, 4, 5, b6, 7"
  (juxt-intervals
   [root major-second minor-third perfect-fourth perfect-fifth minor-sixth major-seventh]))

(def melodic-minor-scale-tones
  "Melodic minor
  functions: 1, 2, b3, 4, 5, 6, 7"
  (juxt-intervals
   [root major-second minor-third perfect-fourth perfect-fifth major-sixth major-seventh]))

(comment
  (print (fret-table-with-tones-p (natural-minor-scale-tones (find-root-p :a))))
  (print (fret-table-with-tones-p (harmonic-minor-scale-tones (find-root-p :a))))
  (print (fret-table-with-tones-p (melodic-minor-scale-tones (find-root-p :a))))
  )

(def minor-pentatonic-scale-tones
  "Minor pentatonic
  functions: 1, b3, 4, 5, b7"
  (juxt-intervals
   [root minor-third perfect-fourth perfect-fifth minor-seventh]))

(def major-pentatonic-scale-tones
  "Major pentatonic
  functions: 1, 2, 3, 5, 7"
  (juxt-intervals
   [root major-second major-third perfect-fifth major-sixth]))

(def minor-pentatonic-blues-scale-tones
  "Minor pentatonic blues
  functions: 1, b3, 4, b5, 5, b7"
  (juxt-intervals
   [root minor-third perfect-fourth diminished-fifth perfect-fifth minor-seventh]))

(comment
  (minor-pentatonic-blues-scale-tones (find-root :e tones))  ;; => [:e :g :a :a# :b :d]
  ;; Prints to REPL
  (print
   (fret-table-with-tones-p
    (minor-pentatonic-blues-scale-tones (find-root :e tones))))
  )

(def doric-scale-tones
  "Doriska skalan
  functions: 1, 2, b3, 4, 5, 6, b7"
  (juxt-intervals
   [root major-second minor-third perfect-fourth perfect-fifth major-sixth minor-seventh]))

(def phrygian-scale-tones
  "Phrygian
  functions: 1, b2, b3, 4, 5, b6, b7"
  (juxt-intervals
   [root minor-second minor-third perfect-fourth perfect-fifth minor-sixth minor-seventh]))

(def lydian-scale-tones
  "Lydian
  functions: 1, 2, 3, #4, 5, 6, 7"
  (juxt-intervals
   [root major-second major-third augmented-fourth perfect-fifth major-sixth major-seventh]))

(def mixolydian-scale-tones
  "Mixolydian
  functions: 1, 2, 3, 4, 5, b7"
  (juxt-intervals
   [root major-second major-third perfect-fourth perfect-fifth major-sixth minor-seventh]))

(def locrian-scale-tones
  "Locrian
  functions: 1, b2, b3, 4, b5, b6, b7"
  (juxt-intervals
   [root minor-second minor-third perfect-fourth diminished-fifth, minor-sixth, minor-seventh]))

(def scales-map
  {:locrian
   {:f         locrian-scale-tones,
    :kw        :locrian,
    :s         "locrian-scale-tones",
    :doc/title "Locrian",
    :doc/fns   "1, b2, b3, 4, b5, b6, b7"},
   :ionian
   {:f         ionian-scale-tones,
    :kw        :ionian,
    :s         "ionian-scale-tones",
    :doc/title "Ionian",
    :doc/fns   "1, 2, 3, 4, 5, 6, 7"},
   :natural-minor
   {:f         natural-minor-scale-tones,
    :kw        :natural-minor,
    :s         "natural-minor-scale-tones",
    :doc/title "Natural minor",
    :doc/fns   "1, 2, b3, 4, 5, b6, b7"},
   :mixolydian
   {:f         mixolydian-scale-tones,
    :kw        :mixolydian,
    :s         "mixolydian-scale-tones",
    :doc/title "Mixolydian",
    :doc/fns   "1, 2, 3, 4, 5, b7"},
   :melodic-minor
   {:f         melodic-minor-scale-tones,
    :kw        :melodic-minor,
    :s         "melodic-minor-scale-tones",
    :doc/title "Melodic minor",
    :doc/fns   "1, 2, b3, 4, 5, 6, 7"},
   :harmonic-minor
   {:f         harmonic-minor-scale-tones,
    :kw        :harmonic-minor,
    :s         "harmonic-minor-scale-tones",
    :doc/title "Harmonic minor",
    :doc/fns   "1, 2, b3, 4, 5, b6, 7"},
   :major
   {:f         major-scale-tones,
    :kw        :major,
    :s         "major-scale-tones",
    :doc/title "Major",
    :doc/fns   "1, 2, 3, 4, 5, 6, 7"},
   :lydian
   {:f         lydian-scale-tones,
    :kw        :lydian,
    :s         "lydian-scale-tones",
    :doc/title "Lydian",
    :doc/fns   "1, 2, 3, #4, 5, 6, 7"},
   :phrygian
   {:f         phrygian-scale-tones,
    :kw        :phrygian,
    :s         "phrygian-scale-tones",
    :doc/title "Phrygian",
    :doc/fns   "1, b2, b3, 4, 5, b6, b7"},
   :minor-pentatonic-blues
   {:f         minor-pentatonic-blues-scale-tones,
    :kw        :minor-pentatonic-blues,
    :s         "minor-pentatonic-blues-scale-tones",
    :doc/title "Minor pentatonic blues",
    :doc/fns   "1, b3, 4, b5, 5, b7"},
   :minor-pentatonic
   {:f         minor-pentatonic-scale-tones,
    :kw        :minor-pentatonic,
    :s         "minor-pentatonic-scale-tones",
    :doc/title "Minor pentatonic",
    :doc/fns   "1, b3, 4, 5, b7"},
   :major-pentatonic
   {:f         major-pentatonic-scale-tones,
    :kw        :major-pentatonic,
    :s         "major-pentatonic-scale-tones",
    :doc/title "Major pentatonic",
    :doc/fns   "1, 2, 3, 5, 7"},
   :minor
   {:f         minor-scale-tones,
    :kw        :minor,
    :s         "minor-scale-tones",
    :doc/title "Natural minor",
    :doc/fns   "1, 2, b3, 4, 5, b6, b7"},
   :aeolian
   {:f         aeolian-scale-tones,
    :kw        :aeolian,
    :s         "aeolian-scale-tones",
    :doc/title "Aeolian",
    :doc/fns   "1, 2, b3, 4, 5, b6, b7"},
   :doric
   {:f         doric-scale-tones,
    :kw        :doric,
    :s         "doric-scale-tones",
    :doc/title "Doriska skalan",
    :doc/fns   "1, 2, b3, 4, 5, 6, b7"}})

(comment
  ;; Generate scales-map
  (->> (ns-publics 'music-theory)
       (filter (comp #(str/includes? % "-scale-tones") str first))
       (map (fn [[k v]]
              (let [doc  (:doc (meta v))
                    docs (str/split-lines doc)
                    kw   (-> (str k)
                             (str/replace "-scale-tones" "")
                             (keyword))]
                [kw
                 {:f         (symbol k)
                  :kw        kw
                  :s         (str k)
                  :doc/title (-> docs first str/trim)
                  :doc/fns   (-> docs second (str/replace "functions:" "") str/trim)}])))
       (into {}))
  )
;; --------------------
;; Scales end
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

(def minor-chord-tones
  "
  short:     m
  full:      minor
  functions: 1 b3 5"
  (juxt-intervals
   [root minor-third perfect-fifth]))

(def sus2-chord-tones
  "
  short:     sus2
  full:      suspended 2
  functions: 1 2 5"
  (juxt-intervals
   [root major-second perfect-fifth]))

(def sus4-chord-tones
  "
  short:     sus2
  full:      suspended 4
  functions: 1 4 5"
  (juxt-intervals
   [root perfect-fourth perfect-fifth]))

(def major-seven-chord-tones
  "
  short:     7
  full:      major seven
  functions: 1 3 5 b7"
  (juxt-intervals
   [root major-third perfect-fifth minor-seventh]))

(comment
  (major-seven-chord-tones tones)            ;; => [:c :e :g :a#]
  (major-seven-chord-tones (find-root-p :c)) ;; => [:c :e :g :a#]
  (major-seven-chord-tones (find-root-p :d)) ;; => [:d :f# :a :c]
  )

(def minor-seven-chord-tones
  "
  short:     m7
  full:      minor seven
  functions: 1 b3 5 b7"
  (juxt-intervals
   [root minor-third perfect-fifth minor-seventh]))

(def minor-maj-seven-chord-tones
  "
  short:     m(maj7)
  full:      minor major seven
  functions: 1 b3 5 7"
  (juxt-intervals
   [root minor-third perfect-fifth major-seventh]))

(def major-maj-seven-chord-tones
  "maj7
  short:     maj7
  full:      major major seven
  functions: 1 3 5 7"
  (juxt-intervals
   [root major-third perfect-fifth major-seventh]))

(def minor-seven-flat-5-chord-tones
  "
  short:     m7b5
  full:      minor sevent flat 5
  functions: 1 b3 b5 b7"
  (juxt-intervals
   [root minor-third diminished-fifth minor-seventh]))

(def major-seven-flat-5-chord-tones
  "
  short:     (maj7)b5
  full:      major major sevent flat 5
  functions: 1 b b5 7"
  (juxt-intervals
   [root major-third diminished-fifth major-seventh]))

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
  functions: 1 b3 b5"
  (juxt-intervals
   [root minor-third diminished-fifth]))

(def diminished-seventh-chord-tones
  "dim7
  short:     dim7
  full:      diminished seventh
  functions: 1 b3 b5 b7"
  (juxt-intervals
   [root minor-third diminished-fifth diminished-seventh]))

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
  ;; Generate chords-map
  (->> (ns-publics 'music-theory)
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

(comment
  (find-chord chords-map tones [:c :g :e]) ;; => "C"
  (find-chord chords-map tones [:b :d :f]) ;; => "Bdim"
  (find-chord chords-map tones [:e :g :b]) ;; => "Em"
  (find-chord-p [:e :g :b])                ;; => "Em"
  )

;; --------------------
;; Chords end
;; --------------------

;; --------------------
;; Harmonization
;; --------------------

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
          (fn [i p m]
            (assoc m :position p :index i))
          (range 1 100)
          ["I" "ii" "iii" "IV" "V" "vi" "vii"]))))

(def harmonizations-p (partial harmonizations scales-map chords-map tones))


(defn harmonization-str [k xs]
  (str
   (->> xs (map (comp #(format "  %-8s" %) :index)) (str/join))
   "\n"
   (->> xs (map (comp #(format "  %-8s" %) :position)) (str/join))
   "\n"
   (->> xs (map (comp #(format "  %-8s" %) :chord-name)) (str/join))
   "\n"
   (->> (if (= k :major)
          ["T" "SD" "T" "SD" "D" "T" "D"]
          ["T" "SD" "T" "SD" "D" "SD" "D"])
        (map #(format "  %-8s" %)) (str/join))
   "\n\n"
   "     T = Tonic, SD = Subdominant, D = Dominant"))

(print
 (fret-table-with-tones-p
  (major-scale-tones tones)))

(comment
  (harmonizations-p :c :major seventh)
  )

;; --------------------
;; Harmonization end
;; --------------------

;; --------------------
;; Org-drill
;; Generate question / answers file
;; --------------------

(defn tones-str [tones]
  (->> tones (map (comp str/upper-case name)) (str/join ", ")))

(spit
 "./music-theory-drills.org"
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
                   "** " (format "%-60s:music:theory:chords:drill:" (str "Tones in " chord-name))
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
               (fret-table-with-tones-p [tone interval-tone])
               "\n")

              (str
               "** " (format "%-60s:music:theory:intervals:drill:" (str "Interval " nr-of-semitones " semitones from " tone-str))
               "\n\n"
               "   " "Name the interval " nr-of-semitones " semitones from " tone-str " and the tone in the interval."
               "\n\n"
               "*** Answer \n\n"
               "    " interval-tone-str
               "\n"
               "    " interval
               "\n\n"
               (fret-table-with-tones-p [tone interval-tone])
               "\n"))))
      (apply str))
     (->> (for [tone                        tones
                [_ {:keys     [s f]
                    :doc/keys [title fns]}] scales-map]
            (let [scale-tones (f (find-root tone tones))
                  tones-str   (->> scale-tones (map (comp str/upper-case name)) (str/join ", "))]
              {:scale-tones scale-tones
               :title       title
               :fns         fns
               :tone        (-> tone name str/upper-case)
               :tones-str   tones-str}))
          (map (fn [{:keys [scale-tones title fns tone tones-str]}]
                 (str
                  "** " (format "%-60s:music:theory:scales:drill:" (str "Tones in " tone " " title " scale"))
                  "\n\n"
                  "   " "What tones are in the " tone " " title " scale?"
                  "\n"
                  "   " "What functions are in the " title " scale?"
                  "\n\n"
                  "*** Answer \n\n"
                  "    " "The tones are: " tones-str
                  "\n"
                  "    " "The functions: " fns
                  "\n\n\n"
                  (fret-table-with-tones-p scale-tones)
                  "\n")))
          (apply str))

     (->> (for [tone    tones
                [k1 s1] [[:major "major"] [:minor "minor"]]
                [k2 s2] [[:triad ""] [:seventh "seventh"]]]
            (let [harmonization      (harmonizations-p tone k1 (get harmonizations-map k2))
                  k (-> harmonization first :scale)
                  harmonization-str' (harmonization-str k harmonization)]
              {:tone              tone
               :tone-str          (-> tone name str/upper-case)
               :m                 s1
               :t                 s2
               :harmonization-str harmonization-str'}))
          (map (fn [{:keys [tone tone-str m t harmonization-str]}]
                 (str
                  "** " (format "%-60s:music:theory:harmonizations:drill:" (str "Diatonic chords in " tone-str " " m " " t))
                  "\n\n"
                  "   Name the diatonic chords in " tone-str " " m " " t " scale."
                  "\n\n"
                  "*** Answer "
                  "\n\n"
                  harmonization-str
                  "\n\n")))
          (apply str))))))

;; --------------------
;; Org-drill end
;; --------------------
