(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.chords :refer [chords defchord]]
   [se.jherrlin.music-theory.utils :refer [docstring->m find-chord find-root fret-table-with-tones juxt-intervals]]))


(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

(def find-root-p #(find-root % tones))

;; ---------------
;; Chords
;; ---------------
(defchord major-chord
  {:a :c :meta :data}
  "1 3 5")

(defchord minor-chord
  "1 b3 5")

(defchord sus2-chord
  "1 2 5")

(defchord sus4-chord
  "1 4 5")

(defchord dominant-seven-chord
  "1 3 5 b7")

(defchord minor-seven-chord
  "1 b3 5 b7")

(defchord minor-maj-seven-chord
  "1 b3 5 7")

(defchord major-maj-seven-chord
  "1 3 5 7")

(defchord minor-seven-flat-5-chord
  "1 b3 b5 b7")

(defchord major-seven-flat-5-chord
  "1 3 b5 7")

(defchord fifth-chord
  "1 5")

(defchord diminished-fifth-chord
  "1 b3 b5")

(defchord diminished-seventh-chord
  "1 b3 b5 bb7")
;; ---------------
;; Chords end
;; ---------------

(comment
  (major (find-root-p :e))
  @chords
  )
