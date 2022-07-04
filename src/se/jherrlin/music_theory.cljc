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
            fret-table-with-tones index-of match-chord-with-scales]
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

(defn fretboard-strings [tuning]
  (utils/fretboard-strings flat-tones sharp-tones tuning))
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
  [[nil              nil            nil            nil]
   [nil              nil            nil            root]
   [nil              nil            perfect-fifth  nil]
   [root             nil            nil            nil]
   [nil              nil            nil            nil]
   [nil              nil            nil            nil]])

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
  )


(interval->tone :c "b5")  ;; => :gb

;; intervals-and-key-to-fretboard-matrix
(defn intervals-and-key-to-fretboard-matrix
  [interval->tone-f tuning key-of intervals fretboard-length]
  (let [
        ;; intervals        ["1" "b3" "5"]
        ;; key-of           :c
        ;; tuning           standard-tuning
        ;; interval->tone-f interval->tone
        ;; fretboard-length 12
        fretboard        (fretboard-strings tuning)
        ]
    (->> fretboard
         (map
          (fn [row]
            (->> row
                 (map
                  (fn [{:keys [flat-tone sharp-tone]}]
                    (->> intervals
                         (map (partial interval->tone-f key-of))
                         (map (fn [t]
                                (condp = t
                                  sharp-tone sharp-tone
                                  flat-tone  flat-tone
                                  nil)))
                         (remove nil?)
                         (first))))
                 (cycle)
                 (take fretboard-length)))))))

(defn intervals-and-key-to-fretboard-matrix-str
  [matrix]
  (let [matrix-with-nrs (concat
                         [(-> matrix first count range)]
                         matrix)
        rows            (->> matrix-with-nrs
                             (map
                              (fn [row]
                                (->> row
                                     (mapv #(cond
                                              (nil? %)    ""
                                              (number? %) (str %)
                                              :else
                                              (utils/tone->str %))))))
                             (map (fn [row]
                                    (apply str (interpose "|" (map #(utils/fformat " %-3s" %) row)))))
                             (map (fn [row]
                                    (str "|" row "|"))))
        row-length      (-> rows first count)]
  (->> (utils/list-insert rows (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
       (str/join "\n"))))

(->> (intervals-and-key-to-fretboard-matrix
      interval->tone
      standard-tuning
      :c
      ["1" "b3" "5"]
      13)
     intervals-and-key-to-fretboard-matrix-str
     println)
















































































{"3"   {:semitones 4, :function "3", :name "Major third"},
 "b3"  {:semitones 3, :function "b3", :name "Minor third"},
 "4"   {:semitones 5, :function "4", :name "perfect fourth"},
 "#5"  {:semitones 8, :function "#5", :name "Augmented fifth"},
 "b4"  {:semitones 4, :function "b4", :name "Diminished fourth"},
 "b7"  {:semitones 10, :function "b7", :name "Minor seventh"},
 "#6"  {:semitones 10, :function "#6", :name "Augmented sixth"},
 "b5"  {:semitones 6, :function "b5", :name "Diminished fifth"},
 "7"   {:semitones 11, :function "7", :name "Major seventh"},
 "bb7" {:semitones 9, :function "bb7", :name "Diminished seventh"},
 "5"   {:semitones 7, :function "5", :name "Perfect fifth"},
 "6"   {:semitones 9, :function "6", :name "Major sixth"},
 "b6"  {:semitones 8, :function "b6", :name "Minor sixth"},
 "1"   {:semitones 0, :function "1", :name "Perfect unison"},
 "b2"  {:semitones 1, :function "b2", :name "Minor second"},
 "#4"  {:semitones 6, :function "#4", :name "Augmented fourth"},
 "2"   {:semitones 2, :function "2", :name "Major second"},
 "#2"  {:semitones 3, :function "#2", :name "Augmented second"},
 "#3"  {:semitones 5, :function "#3", :name "Augmented third"}}
