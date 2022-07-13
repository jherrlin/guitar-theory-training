(ns v2.se.jherrlin.music-theory.definitions
  (:require
   [v2.se.jherrlin.music-theory.intervals :as intervals]
   [v2.se.jherrlin.music-theory.utils :as utils]))


(def all-tones
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
(defn define-chord
  ([chord-id intervals]
   (define-chord chord-id {} intervals))
  ([chord-id meta-data intervals]
   (let [chord (utils/define-chord intervals/intervals-map-by-function chord-id meta-data intervals)
         id    (get chord :chord/id)]
     (swap! chords-atom assoc id chord))))

(defn define-chord-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [chord-pattern (utils/define-chord-pattern pattern-name meta-data pattern)
         id            (get chord-pattern :chord/pattern-id)]
     (swap! chord-patterns-atom assoc id chord-pattern))))

(defn define-scale
  ([scale-id intervals-str]
   (define-scale scale-id {} intervals-str))
  ([scale-id meta-data intervals-str]
   (let [scale (utils/define-scale intervals/intervals-map-by-function scale-id meta-data intervals-str)
         id    (get scale :scale/id)]
     (swap! scales-atom assoc id scale))))

(defn define-mode
  ([pattern-name pattern]
   (define-mode pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [mode (utils/define-mode pattern-name meta-data pattern)
         id   (get mode :mode/id)]
     (swap! modes-atom assoc id mode))))
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
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode :ionian-6
  {:scale :ionian
   :string  6}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode :ionian-5
  {:scale :ionian
   :string  5}
  "-   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode :ionian-4
  {:scale :ionian
   :string  4}
  "-   -   -   -   -
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :ionian-3
  {:scale :ionian
   :string  3}
  "6   -   7   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode :mixolydian
  {:scale  :mixolydian}
  "-   1   -   2   -
   -   5   -   6  b7
   2   -   3   4   -
   6  b7   -   1   -
   3   4   -   5   -
   -   1   -   2   -")

(define-mode :mixolydian-6
  {:scale :mixolydian
   :string  6}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2")

(define-mode :mixolydian-5
  {:scale :mixolydian
   :string  5}
  "-   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode :mixolydian-4
  {:scale :mixolydian
   :string  4}
  "-   -   -   -   -
   -   6  b7   -   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :mixolydian-3
  {:scale  :mixolydian
   :string 3}
  "6  b7   -   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode :aeolian
  {:scale  :aeolian}
  "-   1   -   2  b3
   -   5  b6   -  b7
   2  b3   -   4   -
   -  b7   -   1   -
   -   4   -   5  b6
   -   1   -   2  b3")

(define-mode :aeolian-6
  {:scale  :aeolian
   :string 6}
  "-   -   -   -
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
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode :aeolian-3
  {:scale  :aeolian
   :string 3}
  "-  b7   -   1   -
   -   4   -   5  b6
   1   -   2  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :dorian
  {:scale  :dorian}
  "-   1   -   2  b3
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
  "1  b2   -  b3
   5  b6   -  b7
  b3   -   4   -
  b7   -   1  b2
   4   -   5  b6
   1  b2   -  b3")


(define-mode :phrygian-6
  {:scale  :phrygian}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3")

(define-mode :phrygian-5
  {:scale  :phrygian}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode :phrygian-4
  {:scale  :phrygian}
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode :phrygian-3
  {:scale  :phrygian}
  "-  b7   -   1   -
   -   4   -   5  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode :lydian
  {:scale  :lydian}
  "7   1   -   2
  b5   5   -   6
   2   -   3   -
   6   -   7   1
   3   -  b5   5
   -   1   -   2")

(define-mode :locrian
  {:scale  :locrian}
  "1  b2   -  b3
   -  b6   -  b7
  b3   -   4  b5
  b7   -   1  b2
   4  b5   -  b6
   1  b2   -  b3")

(define-mode :locrian-6
  {:scale  :locrian}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3")

(define-mode :locrian-5
  {:scale  :locrian}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode :locrian-4
  {:scale  :locrian}
  "-   -   -   -
   -  b7   -   1
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode :locrian-3
  {:scale  :locrian}
  "-  b7   -   1   -
   -   4  b5   -  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

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
