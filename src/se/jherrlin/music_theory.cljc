(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.intervals
    :refer [perfect-unison root minor-second major-second
            augmented-second minor-third major-third
            augmented-third diminished-fourth perfect-fourth
            augmented-fourth diminished-fifth perfect-fifth
            augmented-fifth minor-sixth major-sixth augmented-sixth
            diminished-seventh minor-seventh major-seventh
            octave perfect-octave]
    :as intervals]
   [se.jherrlin.music-theory.utils
    :refer [find-chord find-chord-name find-root
            fret-table-with-tones match-chord-with-scales]
    :as utils]
   [clojure.set :as set]))


(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

;; ---------------
;; State / data
;; ---------------
(def chords-atom (atom {}))
@chords-atom

(def scales-atom (atom {}))
@scales-atom

(def chord-patterns-atom (atom {}))
@chord-patterns-atom

(def modes-atom (atom {}))
@modes-atom
;; ---------------
;; State / data end
;; ---------------

;; ---------------
;; Partially applied functions.
;; Presets arguments that can be predefined.
;; ---------------
(def diatonic-chord-progressions-str utils/diatonic-chord-progressions-str)

(def find-root-p #(find-root % tones))

(def fret-table-with-tones-p (partial fret-table-with-tones tones))

(defn find-chord-name-p [chord-tones]
  (find-chord-name @chords-atom tones chord-tones))

(defn find-chord-p [chord-tones]
  (find-chord @chords-atom tones chord-tones))

(defn match-chord-with-scales-p [chord-indexes]
  (match-chord-with-scales @scales-atom chord-indexes))

(defn interval [tones tone i]
  (nth (find-root tone tones) i))

(def interval-p (partial interval tones))

(def triad-or-seven-map
  {:triad   utils/triad
   :seventh utils/seventh})

(defn diatonic-chord-progressions-p [tone scale f]
  (utils/diatonic-chord-progressions triad-or-seven-map @scales-atom @chords-atom tones tone scale f))

(def define-chord
  (partial utils/define-chord
           intervals/intervals-map-by-function chords-atom))
(def define-scale
  (partial utils/define-scale
           intervals/intervals-map-by-function scales-atom))

(def define-chord-pattern
  (partial utils/define-chord-pattern
           chord-patterns-atom))

(def define-mode
  (partial utils/define-mode
           modes-atom))

(defn locate-pattern-on-fret [root-tone mode-spec]
  (utils/locate-pattern-on-fret find-root-p interval-p [:e :b :g :d :a :e] root-tone mode-spec))

(defn chord-pattern-str [modes-atom mode-pattern tone]
  (->> modes-atom mode-pattern :chord/pattern (locate-pattern-on-fret tone)
       utils/padding-fret-pattern
       utils/fret-pattern-to-str))

(defn mode-pattern-str [modes-atom mode-pattern tone]
  (->> modes-atom mode-pattern :mode/pattern (locate-pattern-on-fret tone)
       utils/padding-fret-pattern
       utils/fret-pattern-to-str))

(defn mode-pattern-str-p [mode-pattern tone]
  (mode-pattern-str @modes-atom mode-pattern tone))
;; ---------------
;; Partial functions end.
;; ---------------

;; ---------------
;; Chords
;; ---------------
(define-chord :major
  {:sufix        ""
   :explanation  "major"
   :display-text "Major"}
  "1 3 5")

(define-chord :minor
  {:sufix        "m"
   :explanation  "minor"
   :display-text "Minor"}
  "1 b3 5")

(define-chord :sus2
  {:sufix       "sus2"
   :explanation "suspended 2"}
  "1 2 5")

(define-chord :augmented-triad
  {:sufix       "aug"
   :explanation "augmented triad"}
  "1 3 #5")

(define-chord :sus4
  {:sufix       "sus4"
   :explanation "suspended 4"}
  "1 4 5")

(define-chord :dominant-seven
  {:sufix       "7"
   :explanation "dominant 7th"}
  "1 3 5 b7")

(define-chord :minor-seven
  {:sufix       "m7"
   :explanation "minor 7th"}
  "1 b3 5 b7")

(define-chord :minor-maj-seven
  {:sufix        "m(maj7)"
   :explanation  "minor maj 7th"}
  "1 b3 5 7")

(define-chord :major-maj-seven
  {:sufix       "maj7"
   :explanation "major maj 7th"}
  "1 3 5 7")

(define-chord :minor-seven-flat-5
  {:sufix       "m7b5"
   :explanation "minor seven flat 5"}
  "1 b3 b5 b7")

(define-chord :major-seven-flat-5
  {:sufix        "(maj7)b5"
   :explanation  "major major sevent flat 5"
   :display-text "maj7b5"}
  "1 3 b5 7")

(define-chord :major-seven-sharp-5
  {:sufix        "(maj7)#5"
   :explanation  "major major sevent sharp 5"
   :display-text "maj7#5"}
  "1 3 #5 7")

(define-chord :fifth
  {:sufix       "5"
   :explanation "power chord"}
  "1 5")

(define-chord :diminished-fifth
  {:sufix       "dim"
   :explanation "diminished fifth"}
  "1 b3 b5")

(define-chord :diminished-seventh
  {:sufix       "dim7"
   :explanation "diminished seven"}
  "1 b3 b5 bb7")
;; ---------------
;; Chords end
;; ---------------

;; ---------------
;; Scales
;; ---------------
(define-scale :major
  "1, 2, 3, 4, 5, 6, 7")

(define-scale :minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :harmonic-minor
  "1, 2, b3, 4, 5, b6, 7")

(define-scale :melodic-minor
  "1, 2, b3, 4, 5, 6, 7")

(define-scale :natural-minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :pentatonic-major
  "1, 2, 3, 5, 6")

(define-scale :pentatonic-minor
  "1, b3, 4, 5, b7")

(define-scale :pentatonic-blues
  "1, b3, 4, b5, 5, b7")

(define-scale :pentatonic-neutral
  "1, 2, 4, 5, b7")

(define-scale :ionian
  "1, 2, 3, 4, 5, 6, 7")

(define-scale :dorian
  "1, 2, b3, 4, 5, 6, b7")

(define-scale :phrygian
  "1, b2, b3, 4, 5, b6, b7")

(define-scale :lydian
  "1, 2, 3, #4, 5, 6, 7")

(define-scale :mixolydian
  "1, 2, 3, 4, 5, 6, b7")

(define-scale :aeolian
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :locrian
  "1, b2, b3, 4, b5, b6, b7")

(define-scale :diatonic
  "1, 2, 3, 5, 6")

(define-scale :diminished
  "1, 2, b3, 4, b5, b6, 6, 7")
;; ---------------
;; Scales end
;; ---------------

;; --------------------
;; Modes
;; --------------------
(define-mode :ionian
  {:scale :ionian}
  [[major-seventh root            nil           major-second]    ;; high E
   [nil           perfect-fifth   nil           major-sixth]     ;; B
   [major-second  nil             major-third   perfect-fourth]  ;; G
   [major-sixth   nil             major-seventh root]            ;; D
   [major-third   perfect-fourth  nil           perfect-fifth]   ;; A
   [nil           root            nil           major-second]])  ;; E

(define-mode :ionian-6
  {:scale :ionian
   :string  6}
  [[nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [major-sixth  nil             major-seventh  root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [nil          root            nil            major-second]])

(define-mode :ionian-5
  {:scale :ionian
   :string  5}
  [[nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [major-sixth  nil             major-seventh  root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [nil          root            nil            major-second]
   [nil          nil             nil            nil]])

(define-mode :ionian-4
  {:scale :ionian
   :string  4}
  [[nil          nil             nil            nil            nil]
   [nil          major-sixth     nil            major-seventh  root]
   [major-third  perfect-fourth  nil            perfect-fifth  nil]
   [nil          root            nil            major-second   nil]
   [nil          nil             nil            nil            nil]
   [nil          nil             nil            nil            nil]])

(define-mode :ionian-3
  {:scale :ionian
   :string  3}
  [[major-sixth  nil             major-seventh  root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [root         nil             major-second   nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]])

(define-mode :mixolydian
  {:scale  :mixolydian}
  [[nil           root            nil           major-second   nil]
   [nil           perfect-fifth   nil           major-sixth    minor-seventh]
   [major-second  nil             major-third   perfect-fourth nil]
   [major-sixth   minor-seventh   nil           root           nil]
   [major-third   perfect-fourth  nil           perfect-fifth  nil]
   [nil           root            nil           major-second   nil]])

(define-mode :mixolydian-6
  {:scale :mixolydian
   :string  6}
  [[nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [major-sixth  minor-seventh   nil            root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [nil          root            nil            major-second]])

(define-mode :mixolydian-5
  {:scale :mixolydian
   :string  5}
  [[nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [major-sixth  minor-seventh   nil            root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [nil          root            nil            major-second]
   [nil          nil             nil            nil]])

(define-mode :mixolydian-4
  {:scale :mixolydian
   :string  4}
  [[nil          nil             nil            nil            nil]
   [nil          major-sixth     minor-seventh  nil            root]
   [major-third  perfect-fourth  nil            perfect-fifth  nil]
   [nil          root            nil            major-second   nil]
   [nil          nil             nil            nil            nil]
   [nil          nil             nil            nil            nil]])

(define-mode :mixolydian-3
  {:scale  :mixolydian
   :string 3}
  [[major-sixth  minor-seventh   nil            root]
   [major-third  perfect-fourth  nil            perfect-fifth]
   [root         nil             major-second   nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]
   [nil          nil             nil            nil]])

(define-mode :aeolian
  {:scale  :aeolian}
  [[nil           root            nil          major-second    minor-third]
   [nil           perfect-fifth   minor-sixth  nil             minor-seventh]
   [major-second  minor-third     nil          perfect-fourth  nil]
   [nil           minor-seventh   nil          root            nil]
   [nil           perfect-fourth  nil          perfect-fifth   minor-sixth]
   [nil           root            nil          major-second    minor-third]])

(define-mode :aeolian-6
  {:scale  :aeolian
   :string 6}
  [[nil            nil             nil            nil]
   [nil            nil             nil            nil]
   [nil            nil             nil            nil]
   [minor-seventh  nil             root           nil]
   [perfect-fourth nil             perfect-fifth  minor-sixth]
   [root           nil             major-second   minor-third]])

(define-mode :aeolian-5
  {:scale  :aeolian
   :string 5}
  [[nil            nil             nil            nil]
   [nil            nil             nil            nil]
   [minor-seventh  nil             root           nil]
   [perfect-fourth nil             perfect-fifth  minor-sixth]
   [root           nil             major-second   minor-third]
   [nil            nil             nil            nil]])

(define-mode :aeolian-4
  {:scale  :aeolian
   :string 4}
  [[nil             nil             nil            nil]
   [nil             minor-seventh   nil            root]
   [perfect-fourth  nil             perfect-fifth  minor-sixth]
   [root            nil             major-second   minor-third]
   [nil             nil             nil            nil]
   [nil             nil             nil            nil]])

(define-mode :aeolian-3
  {:scale  :aeolian
   :string 3}
  [[nil             minor-seventh   nil            root           nil]
   [nil             perfect-fourth  nil            perfect-fifth  minor-sixth]
   [root            nil             major-second   minor-third    nil]
   [nil             nil             nil            nil            nil]
   [nil             nil             nil            nil            nil]
   [nil             nil             nil            nil            nil]])

(define-mode :dorian
  {:scale  :dorian}
  [[nil           root            nil  major-second    minor-third]
   [nil           perfect-fifth   nil  major-sixth     minor-seventh]
   [major-second  minor-third     nil  perfect-fourth  nil]
   [major-sixth   minor-seventh   nil  root            nil]
   [nil           perfect-fourth  nil  perfect-fifth   nil]
   [nil           root            nil  major-second    minor-third]])

(define-mode :phrygian
  {:scale  :phrygian}
  [[root            minor-second   nil             minor-third]
   [perfect-fifth   minor-sixth    nil             minor-seventh]
   [minor-third     nil            perfect-fourth  nil]
   [minor-seventh   nil            root            minor-second]
   [perfect-fourth  nil            perfect-fifth   minor-sixth]
   [root            minor-second   nil             minor-third]])

(define-mode :lydian
  {:scale  :lydian}
  [[major-seventh     root           nil              major-second]
   [augmented-fourth  perfect-fifth  nil              major-sixth]
   [major-second      nil            major-third      nil]
   [major-sixth       nil            major-seventh    root]
   [major-third       nil            augmented-fourth perfect-fifth]
   [nil               root           nil              major-second]])

(define-mode :locrian
  {:scale  :locrian}
  [[root            minor-second      nil            minor-third]
   [nil             minor-sixth       nil            minor-seventh]
   [minor-third     nil               perfect-fourth diminished-fifth]
   [minor-seventh   nil               root           minor-second]
   [perfect-fourth  diminished-fifth  nil            minor-sixth]
   [root            minor-second      nil            minor-third]])
;; --------------------
;; Modes end
;; --------------------

;; --------------------
;; Chord patterns
;;
;; Matrixes specifies patterns on how chords looks like and where each interval
;; is located in the matrix. This corresponds to how the chord looks like on the
;; fret board.
;; --------------------
(define-chord-pattern :major-1
  {:name :major}
  [[major-third   nil   nil          nil]     ;; E string
   [nil           root  nil          nil]     ;; B string
   [perfect-fifth nil   nil          nil]     ;; G string
   [nil           nil   major-third  nil]     ;; D string
   [nil           nil   nil          root]    ;; A string
   [nil           nil   nil          nil]])   ;; E string

(define-chord-pattern :major-2
  {:name :major}
  [[perfect-fifth nil nil]
   [nil nil major-third]
   [nil nil root]
   [nil nil perfect-fifth]
   [root nil nil]
   [nil nil nil]])

(define-chord-pattern :major-3
  {:name :major}
  [[root          nil         nil]
   [perfect-fifth nil         nil]
   [nil           major-third nil]
   [nil           nil         root]
   [nil           nil         perfect-fifth]
   [root          nil         nil]])

(define-chord-pattern :minor-1
  {:name :minor}
  [[root          nil nil]
   [perfect-fifth nil nil]
   [minor-third   nil nil]
   [nil           nil root]
   [nil           nil perfect-fifth]
   [root          nil nil]])

(define-chord-pattern :minor-2
  {:name :minor}
  [[perfect-fifth  nil          nil]
   [nil            minor-third  nil]
   [nil            nil          root]
   [nil            nil          perfect-fifth]
   [root           nil          nil]
   [nil            nil          nil]])

(define-chord-pattern :minor-3
  {:name :minor}
  [[nil            minor-third  nil            nil]
   [nil            nil          nil            root]
   [nil            nil          perfect-fifth  nil]
   [root           nil          nil            nil]
   [nil            nil          nil            nil]
   [nil            nil          nil            nil]])

(define-chord-pattern :dominant-seven-1
  {:name :dominant-seven}
  [[root           nil          nil]
   [perfect-fifth  nil          nil]
   [nil            major-third  nil]
   [minor-seventh  nil          nil]
   [nil            nil          perfect-fifth]
   [root           nil          nil]])

(define-chord-pattern :dominant-seven-2
  {:name :dominant-seven}
  [[nil     nil            major-third]
   [nil     minor-seventh  nil]
   [nil     nil            perfect-fifth]
   [root    nil            nil]
   [nil     nil            nil]
   [nil     nil            nil]])

(define-chord-pattern :dominant-seven-3
  {:name :dominant-seven}
  [[perfect-fifth  nil            nil]
   [nil            nil            major-third]
   [minor-seventh  nil            nil]
   [nil            nil            perfect-fifth]
   [root           nil            nil]
   [nil            nil            nil]])


(define-chord-pattern :minor-seven-1
  {:name :minor-seven}
  [[perfect-fifth  nil            nil]
   [nil            minor-third    nil]
   [minor-seventh  nil            nil]
   [nil            nil            perfect-fifth]
   [root           nil            nil]
   [nil            nil            nil]])

(define-chord-pattern :minor-seven-2
  {:name :minor-seven}
  [[root             nil            nil]
   [perfect-fifth    nil            nil]
   [minor-third      nil            nil]
   [minor-seventh    nil            nil]
   [nil              nil            perfect-fifth]
   [root             nil            nil]])

(define-chord-pattern :minor-seven-3
  {:name :minor-seven}
  [[nil             minor-third    nil]
   [nil             minor-seventh  nil]
   [nil             nil            perfect-fifth]
   [root            nil            nil]
   [nil             nil            nil]
   [nil             nil            nil]])


;; --------------------
;; Chord patterns end
;; --------------------

;; --------------------
;; Comment and REPL code
;; --------------------
(comment
  (diatonic-chord-progressions-p :c :major :triad)
  (diatonic-chord-progressions-p :c :minor :triad)
  (diatonic-chord-progressions-p :e :major :seventh)
  (diatonic-chord-progressions-p :e :minor :seventh)

  (->> (diatonic-chord-progressions-p :c :major :triad)
       (map :chord-name))

  (->> (diatonic-chord-progressions-p :c :minor :triad)
       (map :chord-tones)
       (apply concat)
       (set)
       (vec))

  (find-chord-p [:g :b :d# :f#])

  (print
   (diatonic-chord-progressions-str
    (diatonic-chord-progressions-p :c :major :triad)))


  ;; mode to chord
  (for [scale (vals @scales-atom)
        chord (vals @chords-atom)
        :when (set/subset? (set (:chord/indexes chord)) (set (:scale/indexes scale)))]
    (merge chord scale))


  (for [scale (vals @scales-atom)
        chord (vals @chords-atom)
        :when (set/subset? (set (:chord/indexes chord)) (set (:scale/indexes scale)))]
    (merge chord scale))

  (match-chord-with-scales-p [0 4 7])

  (let [chord-indexes [0 4 7]]
    (->> (vals @scales-atom)
         (filter (fn [{:scale/keys [indexes]}]
                   (set/subset? (set chord-indexes) (set indexes))))))

  (locate-pattern-on-fret
   :c
   (-> @modes-atom :ionian :mode/pattern))

  (->> @chord-patterns-atom :major-1 :chord/pattern (locate-pattern-on-fret :c))

  (print
   (chord-pattern-str @chord-patterns-atom :dominant-seven-1 :e))

  (print
   (mode-pattern-str @modes-atom :mixolydian-6 :g))
  )
