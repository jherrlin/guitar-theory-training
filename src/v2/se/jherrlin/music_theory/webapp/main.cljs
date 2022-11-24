(ns v2.se.jherrlin.music-theory.webapp.main
  (:require
   [re-frame.core :as re-frame]
   [v2.se.jherrlin.music-theory.webapp.chords :as chords]
   [v2.se.jherrlin.music-theory.webapp.events :as events]
   [v2.se.jherrlin.music-theory.webapp.harmonizations :as harmonizations]
   [v2.se.jherrlin.music-theory.webapp.modes :as modes]
   [v2.se.jherrlin.music-theory.webapp.scales :as scales]
   [v2.se.jherrlin.music-theory.webapp.settings :as settings]
   [v2.se.jherrlin.music-theory.webapp.neck :as neck]))


(def routes
  [chords/routes
   modes/routes
   scales/routes
   harmonizations/routes
   settings/routes
   neck/routes])
