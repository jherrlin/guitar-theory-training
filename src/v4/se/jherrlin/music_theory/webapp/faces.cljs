(ns v4.se.jherrlin.music-theory.webapp.faces
  (:require
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]))




(def faces
  {:css/piano      piano.view/piano-unit
   ;; :css/fretboard  :b
   :text/fretboard :c})
