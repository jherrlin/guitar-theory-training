(ns v2.se.jherrlin.music-theory.webapp.main
  (:require
   [re-frame.core :as re-frame]
   [v2.se.jherrlin.music-theory.webapp.chords :as chords]
   [v2.se.jherrlin.music-theory.webapp.events :as events]
   [v2.se.jherrlin.music-theory.webapp.harmonizations :as harmonizations]
   [v2.se.jherrlin.music-theory.webapp.modes :as modes]
   [v2.se.jherrlin.music-theory.webapp.scales :as scales]
   [v2.se.jherrlin.music-theory.webapp.settings :as settings]))


(defn on-navigate [new-match]
  (when new-match
    (re-frame/dispatch [::events/navigated new-match])))

(defn index []
  [:div "hejsan"])

(def routes
  ["/v2/"
   [""
    {:name      ::v2-home
     :view      [index]}]
   chords/routes
   modes/routes
   scales/routes
   harmonizations/routes
   settings/routes])
