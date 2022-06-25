(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils
    :refer [docstring->m find-chord-name find-root
            fret-table-with-tones juxt-intervals
            find-chord fformat match-chord-with-scales]
    :as utils]
   [clojure.string :as str]
   [clojure.set :as set]))


(comment
  (remove-ns 'se.jherrlin.music-theory)
  )

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

;; ---------------
;; Partially applied functions.
;; Presets arguments that can be predefined.
;; ---------------
(def chords-atom (atom {}))
@chords-atom
(def scales-atom (atom {}))
@scales-atom

(def find-root-p #(find-root % tones))
(def fret-table-with-tones-p (partial fret-table-with-tones tones))
(defn find-chord-name-p [chord-tones]
  (find-chord-name @chords-atom tones chord-tones))
(defn find-chord-p [chord-tones]
  (find-chord @chords-atom tones chord-tones))
(defn match-chord-with-scales-p [chord-indexes]
  (match-chord-with-scales @scales-atom chord-indexes))



;; ---------------
;; Partial functions end.
;; ---------------
(defn define
  ([state name' intervals]
   (define state name' {} intervals))
  ([state name' meta-data intervals]
   (let [indexes (->> intervals
                      (re-seq #"b{0,2}#{0,2}\d")
                      (mapv #(get-in intervals/intervals-map-by-function [% :semitones])))
         tags    (cond-> #{}
                   (contains? (set indexes) 3)     (conj :minor)
                   (contains? (set indexes) 4)     (conj :major)
                   (str/includes? intervals "bb7") (conj :diminished)
                   (str/includes? intervals "7")   (conj :seventh))]
     (swap! state assoc name'
            (assoc meta-data
                   :id name'
                   :intervals intervals
                   :indexes indexes
                   :title (-> name'
                              name
                              (str/replace "-" " "))
                   :tags tags
                   :f (juxt-intervals indexes))))))

(def define-chord (partial define chords-atom))
(def define-scale (partial define scales-atom))

;; ---------------
;; Chords
;; ---------------
(define-chord :major
  {:sufix       ""
   :explanation "major"}
  "1 3 5")

(define-chord :minor
  {:sufix       "m"
   :explanation "minor"}
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
  {:sufix       "(maj7)"
   :explanation "minor maj 7th"}
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
  {:sufix       "(maj7)b5"
   :explanation "major major sevent flat 5"}
  "1 3 b5 7")

(define-chord :major-seven-sharp-5
  {:sufix       "(maj7)#5"
   :explanation "major major sevent sharp 5"}
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
;; Diatonic-Chord-Progressions
;; --------------------
(def triad   (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def seventh (juxt #(nth % 0) #(nth % 2) #(nth % 4) #(nth % 6)))

(def triad-or-seven-map
  {:triad   triad
   :seventh seventh})

(defn diatonic-chord-progressions [triad-or-seven-map scales-map chords-map all-tones tone kind triad-or-seven]
  {:pre [(keyword? tone) (keyword? kind)]}
  (let [scale-tones ((get-in scales-map [kind :f]) (find-root tone all-tones))]
    (->> scale-tones
         (reduce
          (fn [m t]
            (let [chord-tones ((get triad-or-seven-map triad-or-seven) (find-root t scale-tones))
                  chord-name  (find-chord-name chords-map all-tones chord-tones)]
              (conj m {:key-of      tone
                       :kind        kind
                       :chord-name  chord-name
                       :chord-tones chord-tones})))
          [])
         (mapv
          #(assoc %7 :index %1 :position %2 :mode %3 :mode-str %4 :family %5 :family-str %6)
          (range 1 100)
          (if (= kind :major)
            ["I" "ii" "iii" "IV" "V" "vi" "vii"]
            ["i" "ii" "III" "iv" "v" "VI" "VII"])
          (if (= kind :major)
            [:ionian  :dorian  :phrygian :lydian :mixolydian :aeolian :locrian]
            [:aeolian :locrian :ionian   :dorian :phrygian   :lydian  :mixolydian])
          (if (= kind :major)
            ["Ionian"  "Dorian"  "Phrygian" "Lydian" "Mixolydian" "Aeolian" "Locrian"]
            ["Aeolian" "Locrian" "Ionian"   "Dorian" "Phrygian"   "Lydian"  "Mixolydian"])
          (if (= kind :major)
            [:tonic :subdominant :tonic :subdominant :dominant :tonic :dominant]
            [:tonic :subdominant :tonic :subdominant :dominant :subdominant :dominant])
          (if (= kind :major)
            ["T" "S" "T" "S" "D" "T" "D"]
            ["T" "S" "T" "S" "D" "S" "D"]))
         (mapv (fn [{:keys [chord-tones] :as m}]
                 (assoc m :chord (find-chord-p chord-tones)))))))

(defn diatonic-chord-progressions-p [tone scale f]
  (diatonic-chord-progressions triad-or-seven-map @scales-atom @chords-atom tones tone scale f))

(diatonic-chord-progressions triad-or-seven-map @scales-atom @chords-atom tones :c :major :triad)
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


(defn diatonic-chord-progressions-str [xs]
  (str
   "     T = Tonic (stable), S = Subdominant (leaving), D = Dominant (back home)"
   "\n\n"
   (->> xs (map (comp #(fformat "   %-10s" %) str :index)) (str/join))
   "\n"
   (->> xs (map (comp #(fformat "   %-10s" %) str :position)) (str/join))
   "\n"
   (->> xs (map (comp #(fformat "   %-10s" %) str :mode-str)) (str/join))
   "\n"
   (->> xs (map (comp #(fformat "   %-10s" %) str :family-str)) (str/join))
   "\n"
   (->> xs (map (comp #(fformat "   %-10s" %) str :chord-name)) (str/join))))
;; --------------------
;; Diatonic chord progressions end
;; --------------------




;; mode to chord
(for [scale (vals @scales-atom)
      chord (vals @chords-atom)
      :when (set/subset? (set (:indexes chord)) (set (:indexes scale)))]
  {:chord chord
   :scale scale})

(match-chord-with-scales-p [0 4 7])
