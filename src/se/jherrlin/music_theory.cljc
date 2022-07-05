(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.intervals
    :as intervals]
   [se.jherrlin.music-theory.utils
    :refer [find-chord find-chord-name find-root
            fret-table-with-tones match-chord-with-scales]
    :as utils]
   [clojure.set :as set]
   [clojure.string :as str]))


(def tones           [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
(def sharp-tones     [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
(def flat-tones      [:c :db :d :eb :e :f :gb :g :ab :a :bb :b])
(def standard-tuning [:e :b :g :d :a :e])


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
           chord-patterns-atom
           intervals/intervals-map-by-function))

(def define-mode
  (partial utils/define-mode
           modes-atom
           intervals/intervals-map-by-function))

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

(defn interval->tone
  "Find the correct tone from a tone and interval.

   (interval->tone :c \"b5\") ;; => :gb
   (interval->tone :c \"#5\") ;; => :g#
   (interval->tone :c \"5\")  ;; => :g"
  [tone interval]
  (utils/interval->tone
   intervals/intervals-map-by-function
   #(get-in % [interval :semitones])
   utils/find-root
   sharp-tones
   flat-tones
   tone
   interval))

(comment
  (interval->tone :f# "3")
  (interval->tone :f# "3")
  (interval->tone :a# "b5")
  (interval->tone :e "b3")
  )

(defn fretboard-strings [tuning]
  (utils/fretboard-strings flat-tones sharp-tones tuning))

(defn intervals-and-key-to-fretboard-matrix
  [tuning key-of intervals fretboard-length]
  (utils/intervals-and-key-to-fretboard-matrix
   fretboard-strings
   interval->tone
   tuning key-of intervals fretboard-length))

(def intervals-and-key-to-fretboard-matrix-str
  utils/intervals-and-key-to-fretboard-matrix-str)

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
   :explanation  "major major seven flat 5"
   :display-text "maj7b5"}
  "1 3 b5 7")

(define-chord :major-seven-sharp-5
  {:sufix        "(maj7)#5"
   :explanation  "major major seven sharp 5"
   :display-text "maj7#5"}
  "1 3 #5 7")

(define-chord :fifth
  {:sufix       "5"
   :explanation "5th"}
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

(define-scale :mixolydian-blues-hybrid
  "1, 2, b3, 3, 4, b5, 5, 6, b7")

;; ---------------
;; Scales end
;; ---------------

;; --------------------
;; Modes
;; --------------------
(define-mode :ionian
  {:scale :ionian}
  "
   -   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode :ionian-6
  {:scale :ionian
   :string  6}
  "
   -   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode :ionian-5
  {:scale :ionian
   :string  5}
  "
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode :ionian-4
  {:scale :ionian
   :string  4}
  "
   -   -   -   -   -
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :ionian-3
  {:scale :ionian
   :string  3}
  "
   6   -   7   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode :mixolydian
  {:scale  :mixolydian}
  "
   -   1   -   2   -
   -   5   -   6  b7
   2   -   3   4   -
   6  b7   -   1   -
   3   4   -   5   -
   -   1   -   2   -")

(define-mode :mixolydian-6
  {:scale :mixolydian
   :string  6}
  "
   -   -   -   -
   -   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2")

(define-mode :mixolydian-5
  {:scale :mixolydian
   :string  5}
  "
   -   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode :mixolydian-4
  {:scale :mixolydian
   :string  4}
  "
   -   -   -   -   -
   -   6  b7   -   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :mixolydian-3
  {:scale  :mixolydian
   :string 3}
  "
   6  b7   -   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode :aeolian
  {:scale  :aeolian}
  "
   -   1   -   2  b3
   -   5  b6   -  b7
   2  b3   -   4   -
   -  b7   -   1   -
   -   4   -   5  b6
   -   1   -   2  b3")

(define-mode :aeolian-6
  {:scale  :aeolian
   :string 6}
  "
   -   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3")

(define-mode :aeolian-5
  {:scale  :aeolian
   :string 5}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3
   -   -   -   -")

(define-mode :aeolian-4
  {:scale  :aeolian
   :string 4}
  "
   -   -   -   -
   -  b7   -   1
   4   -   5  b6
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode :aeolian-3
  {:scale  :aeolian
   :string 3}
  "
   -  b7   -   1   -
   -   4   -   5  b6
   1   -   2  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :dorian
  {:scale  :dorian}
  "
   -   1   -   2  b3
   -   5   -   6  b7
   2  b3   -   4   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode :dorian-6
  {:scale  :dorian
   :string 6}
  "-   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode :dorian-5
  {:scale  :dorian
   :string 5}
  "-   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3
   -   -   -   -   -")

(define-mode :dorian-4
  {:scale  :dorian
   :string 4}
  "-   -   -   -
   6  b7   -   1
   4   -   5   -
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode :dorian-3
  {:scale  :dorian
   :string 3}
  "6  b7   -   1
   -   4   -   5
   1   -   2  b3
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode :phrygian
  {:scale  :phrygian}
  "
   1  b2   -  b3
   5  b6   -  b7
  b3   -   4   -
  b7   -   1  b2
   4   -   5  b6
   1  b2   -  b3")

(define-mode :lydian
  {:scale  :lydian}
  "
   7   1   -   2
  b5   5   -   6
   2   -   3   -
   6   -   7   1
   3   -  b5   5
   -   1   -   2")

(define-mode :locrian
  {:scale  :locrian}
  "
   1  b2   -  b3
   -  b6   -  b7
  b3   -   4  b5
  b7   -   1  b2
   4  b5   -  b6
   1  b2   -  b3")

(define-mode :mixolydian-blues-hybrid
  {:scale :mixolydian-blues-hybrid}
  "-   1   -   2  b3
   -   5   -   6  b7
   2  b3   3   4  b5
   6  b7   -   1   -
   3   4  b5   5   -
   -   1   -   2  b3")

;; --------------------
;; Modes end
;; --------------------

;; --------------------
;; Chord patterns
;;
;; Matrixes specifies patterns on how chords looks like and where each interval
;; is located in the matrix. This corresponds to how the chord looks like on the
;; guitar fret board. `nil`s are tones on the fret board where no finger is
;; located.
;; --------------------
(define-chord-pattern :major-1
  {:name :major}
  "
   3   -   -   -
   -   1   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(define-chord-pattern :major-2
  {:name :major}
  "
   5   -   -
   -   -   3
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :major-3
  {:name :major}
  "
   1   -   -
   5   -   -
   -   3   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern :minor-1
  {:name :minor}
  "
   1   -   -
   5   -   -
  b3   -   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern :minor-2
  {:name :minor}
  "
   5   -   -
   -  b3   -
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :minor-3
  {:name :minor}
  "
   -  b3   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern :dominant-seven-1
  {:name :dominant-seven}
  "
   1   -   -
   5   -   -
   -   3   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern :dominant-seven-2
  {:name :dominant-seven}
  "
   -   -   3
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :dominant-seven-3
  {:name :dominant-seven}
  "
   5   -   -
   -   -   3
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :minor-seven-1
  {:name :minor-seven}
  "
   5   -   -
   -  b3   -
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :minor-seven-2
  {:name :minor-seven}
  "
   1   -   -
   5   -   -
  b3   -   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern :minor-seven-3
  {:name :minor-seven}
  "
   -  b3   -
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :fifth-1
  {:name :fifth}
  "-  -  -
   -  -  -
   -  -  -
   -  -  1
   -  -  5
   1  -  -")

(define-chord-pattern :fifth-2
  {:name :fifth}
  "
   -   -   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern :diminished-fifth-1
  {:name :diminished-fifth}
  "-   -   -
   -  b3   -
   -   -   1
   -  b5   -
   1   -   -
   -   -   -")

(define-chord-pattern :diminished-fifth-2
  {:name :diminished-fifth}
  "-   -   -
   -   -   -
   b3  -   -
   -   -   1
   -  b5   -
   1   -   -")

(define-chord-pattern :major-maj-seven-4
  {:name :major-maj-seven}
  "-   -   3
   -   -   7
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :major-maj-seven-6
  {:name :major-maj-seven}
  "1   -   -
   5   -   -
   -   3   -
   -   7   -
   -   -   5
   1   -   -")

(define-chord-pattern :major-maj-seven-5
  {:name :major-maj-seven}
  "5   -   -
   -   -   3
   -   7   -
   -   -   5
   1   -   -
   -   -   -")
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

  (fret-table-with-tones-p
   )

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

  (utils/fretboard-string flat-tones sharp-tones :f#)

  (interval->tone :c "b5")  ;; => :gb

  (->> (intervals-and-key-to-fretboard-matrix
        standard-tuning
        :c
        ["1" "b3" "5"]
        13)
       intervals-and-key-to-fretboard-matrix-str
       println)
  )
