(ns se.jherrlin.drills
  (:require
   [se.jherrlin.drills.chords :as drills.chords]
   [se.jherrlin.drills.intervals :as drills.intervals]))


(def tones-in-chord drills.chords/tones-in-chords)
(def name-the-chord drills.chords/name-the-chord)
(def intervals-in-chord drills.chords/intervals-in-chord)

(def name-the-interval drills.intervals/name-the-interval)
(def tone-in-interval drills.intervals/tone-in-interval)
