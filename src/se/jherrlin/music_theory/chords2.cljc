(ns se.jherrlin.music-theory.chords2
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils :as utils]
   [clojure.string :as str]))







(comment
  (index intervals/intervals-map-by-function "1 3 5")
  (index-p "1 3 5")

  (indexes-p chords-map)
  )













(comment
  chords
  (chord-p :major [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  )
