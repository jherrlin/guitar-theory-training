(ns se.jherrlin.drills
  (:require
   [se.jherrlin.drills.chords :as drills.chords]
   [se.jherrlin.drills.intervals :as drills.intervals]
   [se.jherrlin.drills.modes :as drills.modes]
   [se.jherrlin.drills.tones :as drills.tones]))


(def tones-in-chord drills.chords/tones-in-chords)
(def name-the-chord drills.chords/name-the-chord)
(def intervals-in-chord drills.chords/intervals-in-chord)

(def name-the-interval drills.intervals/name-the-interval)
(def tone-in-interval drills.intervals/tone-in-interval)

(def modes-in-each-key drills.modes/modes-in-each-key)


(def find-all-locations-of-tone drills.tones/find-all-locations-of-tone)
