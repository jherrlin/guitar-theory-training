(ns v4.se.jherrlin.music-theory.definitions
  (:require
   [v4.se.jherrlin.music-theory.utils :as utils]))

(def all-tones (utils/all-tones))

;; Tunings seems to always be defined with the upper (lowest) string first
(def standard-guitar-tuning  [:e :b :g :d :a :e])
(def dadgad                  [:d :a :g :d :a :d])
(def standard-ukulele-tuning [:a :e :c :g])
(def standard-bass-tuning    [:g :d :a :e])
(def banjo-standard          [:a :d :g :c])
(def banjo-irish             [:e :a :d :g])
(def banjo-chicago           [:e :b :g :d])

(def instrument-with-tuning
  {:guitar         {:tuning standard-guitar-tuning
                    :text   "Standard gitar tuning"}
   :dadgad         {:tuning dadgad
                    :text   "DADGAD"}
   :bass           {:tuning standard-bass-tuning
                    :text   "Standard bass tuning"}
   :ukulele        {:tuning standard-ukulele-tuning
                    :text   "Standard ukulele tuning"}
   :banjo-standard {:tuning banjo-standard
                    :text   "Standard tenor tuning"}
   :banjo-irish    {:tuning banjo-irish
                    :text   "Irish tenor tuning"}
   :banjo-chicago  {:tuning banjo-chicago
                    :text   "Chicago tuning"}})

(defn tuning
  "Get tuning and text from key `k`"
  [k]
  (get instrument-with-tuning k))

;; ---------------
;; State / data
;; ---------------
(def chords (atom {}))
@chords

(def scales (atom {}))
@scales

(def chord-patterns (atom {}))
@chord-patterns

(def triad-patterns (atom {}))
@triad-patterns

(def mode-patterns (atom {}))
@mode-patterns

(def scale-patterns (atom {}))
@scale-patterns
;; ---------------
;; State / data end
;; ---------------

;; ---------------
;; Define helpers
;; ---------------
(defn define-chord
  ([chord-id intervals]
   (define-chord chord-id {} intervals))
  ([chord-id meta-data intervals]
   (let [chord (utils/define-chord chord-id meta-data intervals)
         id    (get chord :chord/id)]
     (swap! chords assoc id chord))))

(defn define-scale
  ([scale-id intervals-str]
   (define-scale scale-id {} intervals-str))
  ([scale-id meta-data intervals-str]
   (let [scale (utils/define-scale scale-id meta-data intervals-str)
         id    (get scale :scale/id)]
     (swap! scales assoc id scale))))

(defn define-chord-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :chord)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! chord-patterns assoc id chord-pattern))))

(defn define-triad-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :triad)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! triad-patterns assoc id chord-pattern))))

(defn define-mode-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :mode)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! mode-patterns assoc id chord-pattern))))

(defn define-scale-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :scale)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! scale-patterns assoc id chord-pattern))))

;; ---------------
;; Define helpers end.
;; ---------------

;; ---------------
;; Chords
;; ---------------
(define-chord :major
  {:sufix        ""
   :display-text "major"
   :explanation  "major"}
  "1 3 5")

(define-chord :major-plus-5
  {:sufix        "+5"
   :explanation  "major where 5 is raised one half tone"}
  "1 3 #5")

(define-chord :minor
  {:sufix        "m"
   :display-text "minor"
   :explanation  "minor"}
  "1 b3 5")

(define-chord :major-maj-seven
  {:sufix       "maj7"
   :explanation "major maj 7th"}
  "1 3 5 7")

(define-chord :dominant-seven
  {:sufix       "7"
   :explanation "dominant 7th"}
  "1 3 5 b7")

(define-chord :dominant-seven-sharp-nine
  {:sufix       "7#9"
   :explanation "dominant 7th with a sharp nine"
   :text        "Also knows as the Hendrix chord."}
  "1 3 5 b7 #9")

(define-chord :dominant-seven-flat-nine
  {:sufix       "7b9"
   :explanation "dominant 7th"}
  "1 3 5 b7 b9")

(define-chord :minor-maj-seven
  {:sufix       "m(maj7)"
   :explanation "minor maj 7th"}
  "1 b3 5 7")

(define-chord :minor-seven
  {:sufix       "m7"
   :explanation "minor 7th"}
  "1 b3 5 b7")

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

(define-chord :sus4-add-7
  {:sufix       "7sus4"
   :explanation "suspended 4 with added b7"}
  "1 4 5 b7")

(define-chord :minor-seven-flat-5
  {:sufix       "m7b5"
   :text        "diminished half, often sufixed with Ø"
   :explanation "minor seven flat 5"}
  "1 b3 b5 b7")

(define-chord :major-seven-flat-5
  {:sufix       "maj7b5"
   :explanation "major major seven flat 5"}
  "1 3 b5 7")

(define-chord :major-seven-sharp-5
  {:sufix             "(maj7)#5"
   :explanation       "major major seven sharp 5"
   :sufix-alternative "maj7#5"}
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
   :explanation "diminished seven"
   :text        "diminished whole, often sufixed with °"}
  "1 b3 b5 bb7")

(define-chord :sixth
  {:sufix       "6"
   :explanation "sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 3 5 6")

(define-chord :minor-sixth
  {:sufix       "m6"
   :explanation "minor sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 b3 5 6")

(define-chord :ninth
  {:sufix       "9"
   :explanation "ninth"
   :text        "This is a dominant chord. The most important tones are 1, 3, b7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 b7 9")

(define-chord :maj-ninth
  {:sufix       "maj9"
   :explanation "major ninth"
   :text        "The most important tones are 1, 3, 7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 7 9")

(define-chord :minor-ninth
  {:sufix       "m9"
   :explanation "minor ninth # fifth is the least important tone, it may be ignored"
   :text        "The most important tones are 1, b3, b7 and 9. The 5 can be ignored in the chord."}
  "1 b3 5 b7 9")

(define-chord :minor-add9
  {:sufix       "m(add9)"
   :explanation "minor with an added 9"}
  "1 b3 5 9")

(define-chord :minor-flat6
  {:sufix       "mb6"
   :explanation "minor with an added flat 6"}
  "1 b3 5 b6")

(define-chord :minor-sixth-added9
  {:sufix       "m6/9"
   :explanation "minor sixth with an added 9"}
  "1 b3 5 6 9")

(define-chord :maj-eleventh
  {:sufix       "maj11"
   :explanation "major eleventh"}
  "1 3 5 7 9 11")

(define-chord :eleventh
  {:sufix       "11"
   :explanation "dominant 11"}
  "1 3 5 b7 9 11")

(define-chord :minor-eleventh
  {:sufix       "m11"
   :explanation "minor eleventh"}
  "1 b3 5 b7 9 11")

(define-chord :thirteen
  {:sufix       "13"
   :explanation "thirteenth. Dominant"}
  "1 3 5 b7 9 11 13")

(define-chord :maj-thirteen
  {:sufix "13"
   :explanation "major thirteenth"}
  "1 3 5 7 9 11 13")

(define-chord :minor-thirteen
  {:sufix "13"
   :explanation "major thirteenth"}
  "1 b3 5 7 9 11 13")

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

(define-scale :lydian
  "1, 2, 3, #4, 5, 6, 7")

(define-scale :ionian
  "1, 2, 3, 4, 5, 6, 7")

(define-scale :mixolydian
  "1, 2, 3, 4, 5, 6, b7")

(define-scale :dorian
  "1, 2, b3, 4, 5, 6, b7")

(define-scale :aeolian
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :phrygian
  "1, b2, b3, 4, 5, b6, b7")

(define-scale :locrian
  "1, b2, b3, 4, b5, b6, b7")

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

(define-scale :diatonic
  "1, 2, 3, 5, 6")

(define-scale :diminished
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale :mixolydian-blues-hybrid
  "1, 2, b3, 3, 4, b5, 5, 6, b7")

(define-scale :diminished-half
  "1, b2, b3, 3, b5, 5, 6, b7")

(define-scale :diminished-whole
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale :diminished-whole-tone
  "1, b2, b3, 3, b5, b6, b7")

(define-scale :dominant-7th
  "1, 2, 3, 4, 5, 6, b7")

(define-scale :lydian-augmented
  "1, 2, 3, #4, #5, 6, 7")

(define-scale :lydian-minor
  "1, 2, 3, #4, 5, b6, b7")

(define-scale :lydian-diminished
  "1, 2, b3, #4, 5, 6, 7")
;; ---------------
;; Scales end
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
  {:name   :major
   :tuning standard-guitar-tuning}
  "3   -   -   -
   -   1   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(define-chord-pattern :major-10
  {:name   :major
   :tuning standard-ukulele-tuning}
  "-   -   -   1
   3   -   -   -
   1   -   -   -
   5   -   -   -")

(define-chord-pattern :major-11
  {:name   :major
   :tuning standard-ukulele-tuning}
  "1   -   -
   5   -   -
   -   3   -
   -   -   1")

(define-chord-pattern :major-2
  {:name   :major
   :tuning standard-guitar-tuning}
  "5   -   -
   -   -   3
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :major-3
  {:name   :major
   :tuning standard-guitar-tuning}
  "1   -   -
   5   -   -
   -   3   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern :major-4
  {:name   :major
   :tuning standard-guitar-tuning}
  "-   -   -   1
   3   -   -   -
   1   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1")

(define-chord-pattern :major-plus-5-1
  {:name   :major-plus-5
   :tuning standard-guitar-tuning}
  "1   -   -   -
   -  #5   -   -
   -   3   -   -
   -   -   1   -
   -   -   -  #5
   1   -   -   -")

(define-chord-pattern :minor-10
  {:name   :minor
   :tuning standard-ukulele-tuning}
  "1   -   -
   5   -   -
  b3   -   -
   -   -   1")

(define-chord-pattern :minor-11
  {:name   :minor
   :tuning standard-ukulele-tuning}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   5")

(define-chord-pattern :minor-1
  {:name   :minor
   :tuning standard-guitar-tuning}
  "1   -   -
   5   -   -
  b3   -   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern :minor-2
  {:name   :minor
   :tuning standard-guitar-tuning}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :minor-3
  {:name   :minor
   :tuning standard-guitar-tuning}
  "-  b3   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern :dominant-seven-10
  {:name   :dominant-seven
   :tuning standard-ukulele-tuning}
  "-  b7
   3   -
   1   -
   5   -")

(define-chord-pattern :dominant-seven-11
  {:name   :dominant-seven
   :tuning standard-ukulele-tuning}
  "-   -   3
   -  b7   -
   -   -   5
   1   -   -")

(define-chord-pattern :dominant-seven-1
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "1   -   -
   5   -   -
   -   3   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern :dominant-seven-2
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "-   -   3
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :dominant-seven-3
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "5   -   -
   -   -   3
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :dominant-seven-4
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "1   -   -   -
   -   -   -  b7
   -   3   -   -
   -   -   1   -
   -   -   5   -
   1   -   -   -")

(define-chord-pattern :dominant-seven-5
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "-   -   -  b7
   -   -   3   -
   -   -   1   -
   -   -   5   -
   1   -   -   -
   -   -   -   -")

(define-chord-pattern :dominant-seven-6
  {:name   :dominant-seven
   :tuning standard-guitar-tuning}
  "-   -   -
   1   -   -
   -   -  b7
   -   3   -
   -   -   1
   -   -   -")

(define-chord-pattern :dominant-seven-sharp-nine-1
  {:name   :dominant-seven-sharp-nine
   :tuning standard-guitar-tuning}
  "-   -
   -  #9
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern :dominant-seven-flat-nine-1
  {:name   :dominant-seven-flat-nine
   :tuning standard-guitar-tuning}
  "-   -
  b9   -
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern :minor-seven-1
  {:name   :minor-seven
   :tuning standard-guitar-tuning}
  "
   5   -   -
   -  b3   -
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :minor-seven-2
  {:name   :minor-seven
   :tuning standard-guitar-tuning}
  "
   1   -   -
   5   -   -
  b3   -   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern :minor-seven-3
  {:name   :minor-seven
   :tuning standard-guitar-tuning}
  "
   -  b3   -
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :minor-seven-4
  {:name   :minor-seven
   :tuning standard-guitar-tuning}
  "
   -
   5
  b3
  b7
   -
   1")

(define-chord-pattern :fifth-1
  {:name   :fifth
   :tuning standard-guitar-tuning}
  "-  -  -
   -  -  -
   -  -  -
   -  -  1
   -  -  5
   1  -  -")

(define-chord-pattern :fifth-2
  {:name   :fifth
   :tuning standard-guitar-tuning}
  "
   -   -   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern :diminished-fifth-1
  {:name   :diminished-fifth
   :tuning standard-guitar-tuning}
  "-   -   -
   -  b3   -
   -   -   1
   -  b5   -
   1   -   -
   -   -   -")

(define-chord-pattern :diminished-fifth-2
  {:name   :diminished-fifth
   :tuning standard-guitar-tuning}
  "-   -   -
   -   -   -
   b3  -   -
   -   -   1
   -  b5   -
   1   -   -")

(define-chord-pattern :major-maj-seven-10
  {:name   :major-maj-seven
   :tuning standard-ukulele-tuning}
  "7   -   -   -
   -   5   -   -
   -   -   3   -
   -   -   -   1")

(define-chord-pattern :major-maj-seven-6
  {:name   :major-maj-seven
   :tuning standard-guitar-tuning}
  "1   -   -
   5   -   -
   -   3   -
   -   7   -
   -   -   5
   1   -   -")

(define-chord-pattern :major-maj-seven-5
  {:name   :major-maj-seven
   :tuning standard-guitar-tuning}
  "5   -   -
   -   -   3
   -   7   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :major-maj-seven-4
  {:name   :major-maj-seven
   :tuning standard-guitar-tuning}
  "-   -   3
   -   -   7
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :major-maj-seven-3
  {:name   :major-maj-seven
   :tuning standard-guitar-tuning}
  "-   -
   5   -
   -   3
   -   7
   -   -
   1   -")

(define-chord-pattern :sixth-1
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "1   -   -
   -   -   6
   -   3   -
   -   -   1
   -   -   5
   -   -   -")

(define-chord-pattern :sixth-2
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "-   -   6
   -   -   3
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern :sixth-3
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "6   -   -
   3   -   -
   1   -   -
   5   -   -
   -   -   3
   6   -   -")

(define-chord-pattern :sixth-4
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "-   1   -
   -   5   -
   -   -   3
   6   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :sixth-5
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "-   -   3
   6   -   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :sixth-6
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "3   -   -
   -   1   -
   5   -   -
   -   -   3
   6   -   -
   3   -   -")

(define-chord-pattern :sixth-7
  {:name   :sixth
   :tuning standard-guitar-tuning}
  "-   -   5
   1   -   -
   -   6   -
   -   3   -
   -   -   -
   -   -   -")

(define-chord-pattern :ninth-1
  {:name   :ninth
   :tuning standard-guitar-tuning}
  "-   5
   -   9
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern :ninth-2
  {:name   :ninth
   :tuning standard-guitar-tuning}
  "-   9   -
   -   -  b7
   3   -   -
   -   1   -
   -   -   -
   -   -   -")

(define-chord-pattern :ninth-3
  {:name   :ninth
   :tuning standard-guitar-tuning}
  "-   -
   -   -
   9   -
   -  b7
   3   -
   -   1")

(define-chord-pattern :minor-seven-flat-5-1
  {:name   :minor-seven-flat-5
   :tuning standard-guitar-tuning}
  "-   -
  b5   -
   -  b3
   -  b7
   -   -
   -   1")

(define-chord-pattern :minor-seven-flat-5-2
  {:name   :minor-seven-flat-5
   :tuning standard-guitar-tuning}
  "-   -
   -  b3
  b7   -
   -  b5
   1   -
   -   -")

(define-chord-pattern :minor-seven-flat-5-3
  {:name   :minor-seven-flat-5
   :tuning standard-guitar-tuning}
  "-   1
  b5   -
   -  b3
   -  b7
   -   -
   -   -")

(define-chord-pattern :minor-seven-flat-5-4
  {:name   :minor-seven-flat-5
   :tuning standard-guitar-tuning}
  "-  b3
   -  b7
   -  b5
   1   -
   -   -
   -   -")

(define-chord-pattern :minor-seven-flat-5-5
  {:name   :minor-seven-flat-5
   :tuning standard-guitar-tuning}
  "-  b5   -
   1   -   -
   -   -  b7
  b3   -   -
   -   -   -
   -   -   -")

(define-chord-pattern :minor-ninth-1
  {:name   :minor-ninth
   :tuning standard-guitar-tuning}
  "-   -   -
   -   -   9
   -   -  b7
  b3   -   -
   -   -   1
   -   -   -")

(define-chord-pattern :eleventh-1
  {:name   :eleventh
   :tuning standard-guitar-tuning}
  "5
   9
  b7
  11
   1
   -")

(define-chord-pattern :eleventh-2
  {:name   :eleventh
   :tuning standard-guitar-tuning}
  "-   -   -
  11   -   -
   -   9   -
   -   -  b7
   -   -   -
   -   -   1")
;; --------------------
;; Chord patterns end
;; --------------------

;; ---------------
;; Triad patterns
;; ---------------
(define-triad-pattern :major-1
  {:name   :major
   :tuning standard-guitar-tuning
   :order 1}
  "3   -
   -   1
   5   -
   -   -
   -   -
   -   -")

(define-triad-pattern :major-2
  {:name   :major
   :tuning standard-guitar-tuning
   :order 2}
  "5   -   -
   -   -   3
   -   -   1
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern :major-3
  {:name   :major
   :tuning standard-guitar-tuning
   :order 3}
  "1   -   -
   5   -   -
   -   3   -
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern :major-4
  {:name   :major
   :tuning standard-guitar-tuning
   :order 4}
  "-
   3
   1
   5
   -
   -")

(define-triad-pattern :major-5
  {:name   :major
   :tuning standard-guitar-tuning
   :order 5}
  "-   -   -
   5   -   -
   -   3   -
   -   -   1
   -   -   -
   -   -   -")

(define-triad-pattern :major-6
  {:name   :major
   :tuning standard-guitar-tuning
   :order 6}
  "-   -   -
   -   1   -
   5   -   -
   -   -   3
   -   -   -
   -   -   -")

(define-triad-pattern :major-7
  {:name   :major
   :tuning standard-guitar-tuning
   :order 7}
  "-   -   -
   -   -   -
   3   -   -
   -   1   -
   -   5   -
   -   -   -")

(define-triad-pattern :major-8
  {:name   :major
   :tuning standard-guitar-tuning
   :order 8}
  "-   -   -   -
   -   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(define-triad-pattern :major-9
  {:name   :major
   :tuning standard-guitar-tuning
   :order 9}
  "-   -   -
   -   -   -
   1   -   -
   5   -   -
   -   -   3
   -   -   -")

(define-triad-pattern :major-10
  {:name   :major
   :tuning standard-guitar-tuning
   :order 10}
  "-   -   -
   -   -   -
   -   -   -
   1   -   -
   5   -   -
   -   -   3")

(define-triad-pattern :major-11
  {:name   :major
   :tuning standard-guitar-tuning
   :order 11}
  "-   -
   -   -
   -   -
   3   -
   -   1
   -   5")

(define-triad-pattern :major-12
  {:name   :major
   :tuning standard-guitar-tuning
   :order  12}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1")

;; Minor
(define-triad-pattern :minor-1
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  1}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern :minor-2
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  2}
  "1
   5
  b3
   -
   -
   -")

(define-triad-pattern :minor-3
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  3}
  "b3   -   -
    -   -   1
    -   5   -
    -   -   -
    -   -   -
    -   -   -")

(define-triad-pattern :minor-4
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  4}
  " -   -
    -   1
    5   -
    -  b3
    -   -
    -   -")

(define-triad-pattern :minor-5
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  5}
  " -   -
   b3   -
    -   1
    -   5
    -   -
    -   -")

(define-triad-pattern :minor-6
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  6}
  " -   -   -
    5   -   -
   b3   -   -
    -   -   1
    -   -   -
    -   -   -")

(define-triad-pattern :minor-7
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  7}
  " -   -   -   -
    -   -   -   -
    5   -   -   -
    -  b3   -   -
    -   -   -   1
    -   -   -   -")

(define-triad-pattern :minor-8
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  8}
  " -   -
    -   -
    1   -
    5   -
    -  b3
    -   -")

(define-triad-pattern :minor-9
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  9}
  " -   -   -
    -   -   -
   b3   -   -
    -   -   1
    -   -   5
    -   -   -")

(define-triad-pattern :minor-10
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  10}
  " -   -   -
    -   -   -
    -   -   -
   b3   -   -
    -   -   1
    -   -   5")

(define-triad-pattern :minor-11
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  11}
  " -   -   -   -
    -   -   -   -
    -   -   -   -
    5   -   -   -
    -  b3   -   -
    -   -   -   1")

(define-triad-pattern :minor-12
  {:name   :minor
   :tuning standard-guitar-tuning
   :order  12}
  " -   -
    -   -
    -   -
    1   -
    5   -
    -  b3")
;; ---------------
;; Triad patterns end
;; ---------------

;; --------------------
;; Modes
;; --------------------
(define-mode-pattern :ionian
  {:scale  :ionian
   :tuning standard-guitar-tuning}
  "7   1   -   2
   -   5   -   6
   2   -   3   4
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern :ionian-6
  {:scale  :ionian
   :string 6
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern :ionian-5
  {:scale  :ionian
   :string 5
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern :ionian-4
  {:scale  :ionian
   :string 4
   :tuning standard-guitar-tuning}
  "-   -   -   -   -
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :ionian-3
  {:scale  :ionian
   :string 3
   :tuning standard-guitar-tuning}
  "6   -   7   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :mixolydian
  {:scale  :mixolydian
   :tuning standard-guitar-tuning}
  "-   1   -   2   -
   -   5   -   6  b7
   2   -   3   4   -
   6  b7   -   1   -
   3   4   -   5   -
   -   1   -   2   -")

(define-mode-pattern :mixolydian-6
  {:scale  :mixolydian
   :string 6
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern :mixolydian-5
  {:scale  :mixolydian
   :string 5
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern :mixolydian-4
  {:scale  :mixolydian
   :string 4
   :tuning standard-guitar-tuning}
  "-   -   -   -   -
   -   6  b7   -   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :mixolydian-3
  {:scale  :mixolydian
   :string 3
   :tuning standard-guitar-tuning}
  "6  b7   -   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :aeolian
  {:scale  :aeolian
   :tuning standard-guitar-tuning}
  "-   1   -   2  b3
   -   5  b6   -  b7
   2  b3   -   4   -
   -  b7   -   1   -
   -   4   -   5  b6
   -   1   -   2  b3")

(define-mode-pattern :aeolian-6
  {:scale  :aeolian
   :string 6
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3")

(define-mode-pattern :aeolian-5
  {:scale  :aeolian
   :string 5
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3
   -   -   -   -")

(define-mode-pattern :aeolian-4
  {:scale  :aeolian
   :string 4
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :aeolian-3
  {:scale  :aeolian
   :string 3
   :tuning standard-guitar-tuning}
  "-  b7   -   1   -
   -   4   -   5  b6
   1   -   2  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :dorian
  {:scale  :dorian
   :tuning standard-guitar-tuning}
  "-   1   -   2  b3
   -   5   -   6  b7
   2  b3   -   4   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode-pattern :dorian-6
  {:scale  :dorian
   :string 6
   :tuning standard-guitar-tuning}
  "-   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode-pattern :dorian-5
  {:scale  :dorian
   :string 5
   :tuning standard-guitar-tuning}
  "-   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3
   -   -   -   -   -")

(define-mode-pattern :dorian-4
  {:scale  :dorian
   :string 4
   :tuning standard-guitar-tuning}
  "-   -   -   -
   6  b7   -   1
   4   -   5   -
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :dorian-3
  {:scale  :dorian
   :string 3
   :tuning standard-guitar-tuning}
  "6  b7   -   1
   -   4   -   5
   1   -   2  b3
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :phrygian
  {:scale  :phrygian
   :tuning standard-guitar-tuning}
  "1  b2   -  b3
   5  b6   -  b7
  b3   -   4   -
  b7   -   1  b2
   4   -   5  b6
   1  b2   -  b3")

(define-mode-pattern :phrygian-6
  {:scale  :phrygian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3")

(define-mode-pattern :phrygian-5
  {:scale  :phrygian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode-pattern :phrygian-4
  {:scale  :phrygian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :phrygian-3
  {:scale  :phrygian
   :tuning standard-guitar-tuning}
  "-  b7   -   1   -
   -   4   -   5  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :lydian
  {:scale  :lydian
   :tuning standard-guitar-tuning}
  "7   1   -   2
  b5   5   -   6
   2   -   3   -
   6   -   7   1
   3   -  b5   5
   -   1   -   2")

(define-mode-pattern :lydian-6
  {:scale  :lydian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   -  #4   5
   -   1   -   2")

(define-mode-pattern :lydian-5
  {:scale  :lydian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   6   -   7   1
   3   -  #4   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern :lydian-4
  {:scale  :lydian
   :tuning standard-guitar-tuning}
  "-   -   -   -   -
   -   6   -   7   1
   3   -  #4   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :lydian-3
  {:scale  :lydian
   :tuning standard-guitar-tuning}
  "6   -   7   1
   3   -  #4   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :locrian
  {:scale  :locrian
   :tuning standard-guitar-tuning}
  "1  b2   -  b3
   -  b6   -  b7
  b3   -   4  b5
  b7   -   1  b2
   4  b5   -  b6
   1  b2   -  b3")

(define-mode-pattern :locrian-6
  {:scale  :locrian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3")

(define-mode-pattern :locrian-5
  {:scale  :locrian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode-pattern :locrian-4
  {:scale  :locrian
   :tuning standard-guitar-tuning}
  "-   -   -   -
   -  b7   -   1
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern :locrian-3
  {:scale  :locrian
   :tuning standard-guitar-tuning}
  "-  b7   -   1   -
   -   4  b5   -  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern :mixolydian-blues-hybrid
  {:scale  :mixolydian-blues-hybrid
   :tuning standard-guitar-tuning}
  "-   1   -   2  b3
   -   5   -   6  b7
   2  b3   3   4  b5
   6  b7   -   1   -
   3   4  b5   5   -
   -   1   -   2  b3")
;; --------------------
;; Modes end
;; --------------------

;; ---------------
;; Scales patterns
;; ---------------
(define-scale-pattern :pentatonic-blues-1
  {:scale  :pentatonic-blues
   :tuning standard-guitar-tuning
   :order  1}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   5   -
   1   -   -  b3")

(define-scale-pattern :pentatonic-blues-2
  {:scale  :pentatonic-blues
   :tuning standard-guitar-tuning
   :order  2}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   5   -
   1   -   -  b3
   -   -   -   -")

(define-scale-pattern :pentatonic-blues-3
  {:scale  :pentatonic-blues
   :tuning standard-guitar-tuning
   :order  3}
  "-   -   -   -
   -  b7   -   1
   4  b5   5   -
   1   -   -  b3
   -   -   -   -
   -   -   -   -")

(define-scale-pattern :pentatonic-blues-4
  {:scale  :pentatonic-blues
   :tuning standard-guitar-tuning
   :order  4}
  "-  b7   -   1
   -   4  b5   5
   1   -   -  b3
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-scale-pattern :major-1
  {:scale  :major
   :tuning standard-ukulele-tuning
   :order  1}
  "-   2   -   3   4
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -")

(define-scale-pattern :major-2
  {:scale  :major
   :tuning standard-ukulele-tuning
   :order  2}
  "6   -   7   1   -
   3   4   -   5   -
   1   -   2   -   -
   -   -   -   -   -")
;; ---------------
;; Scales patterns end
;; ---------------
