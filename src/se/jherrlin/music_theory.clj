(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.scales :refer [scales defscale]]
   [se.jherrlin.music-theory.chords :refer [chords defchord]]
   [se.jherrlin.music-theory.utils :refer [docstring->m find-chord find-root fret-table-with-tones juxt-intervals]]
   [clojure.set :as set]))

(comment
  (remove-ns 'se.jherrlin.music-theory)
  )

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

(def find-root-p #(find-root % tones))
(def fret-table-with-tones-p (partial fret-table-with-tones tones))

;; ---------------
;; Chords
;; ---------------
(defchord major-chord
  {:sufix       ""
   :explanation "major"}
  "1 3 5")

(defchord minor-chord
  {:sufix       "m"
   :explanation "minor"}
  "1 b3 5")

(defchord sus2-chord
  {:sufix       "sus2"
   :explanation "suspended 2"}
  "1 2 5")

(defchord sus4-chord
  {:sufix       "sus4"
   :explanation "suspended 4"}
  "1 4 5")

(defchord dominant-seven-chord
  {:sufix       "7"
   :explanation "dominant 7th"}
  "1 3 5 b7")

(defchord minor-seven-chord
  {:sufix       "m7"
   :explanation "minor 7th"}
  "1 b3 5 b7")

(defchord minor-maj-seven-chord
  {:sufix       "(maj7)"
   :explanation "minor maj 7th"}
  "1 b3 5 7")

(defchord major-maj-seven-chord
  {:sufix       "maj7"
   :explanation "major maj 7th"}
  "1 3 5 7")

(defchord minor-seven-flat-5-chord
  {:sufix       "m7b5"
   :explanation "minor seven flat 5"}
  "1 b3 b5 b7")

(defchord major-seven-flat-5-chord
  {:sufix       "(maj7)b5"
   :explanation "major major sevent flat 5"}
  "1 3 b5 7")

(defchord fifth-chord
  {:sufix       "5"
   :explanation "power chord"}
  "1 5")

(defchord diminished-fifth-chord
  {:sufix       "dim"
   :explanation "diminished fifth"}
  "1 b3 b5")

(defchord diminished-seventh-chord
  {:sufix       "dim7"
   :explanation "diminished seven"}
  "1 b3 b5 bb7")

(comment
  (major-chord (find-root-p :c))
  (minor-chord (find-root-p :c))

  @chords

  (find-chord @chords tones [:c :e :g])
  (find-chord @chords tones [:c :d# :g])
  )
;; ---------------
;; Chords end
;; ---------------

;; ---------------
;; Scales
;; ---------------
(defscale major-scale
  "1, 2, 3, 4, 5, 6, 7")

(defscale harmonic-minor-scale
  "1, 2, b3, 4, 5, b6, 7")

(defscale melodic-minor-scale
  "1, 2, b3, 4, 5, 6, 7")

(defscale natural-minor-scale
  "1, 2, b3, 4, 5, b6, b7")

(defscale pentatonic-major-scale
  "1, 2, 3, 5, 6")

(defscale pentatonic-minor-scale
  "1, b3, 4, 5, b7")

(defscale pentatonic-blues-scale
  "1, b3, 4, b5, 5, b7")

(defscale pentatonic-neutral-scale
  "1, 2, 4, 5, b7")

(defscale ionian-scale
  "1, 2, 3, 4, 5, 6, 7")

(defscale dorian-scale
  "1, 2, b3, 4, 5, 6, b7")

(defscale phrygian-scale
  "1, b2, b3, 4, 5, b6, b7")

(defscale lydian-scale
  "1, 2, 3, #4, 5, 6, 7")

(defscale mixolydian-scale
  "1, 2, 3, 4, 5, 6, b7")

(defscale aeolian-scale
  "1, 2, b3, 4, 5, b6, b7")

(defscale locrian-scale
  "1, b2, b3, 4, b5, b6, b7")

(defscale diatonic-scale
  "1, 2, 3, 5, 6")

(defscale diminished-scale
  "1, 2, b3, 4, b5, b6, 6, 7")


(comment
  @scales

  (diminished-scale
   (find-root-p :e))
  )
;; ---------------
;; Scales end
;; ---------------


(for [scale (vals @scales)
      chord (vals @chords)
      :when (set/subset? (set (:indexes chord)) (set (:indexes scale)))]
  {:chord chord
   :scale scale})
