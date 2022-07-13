(ns v2.se.jherrlin.music-theory.webapp.main
  (:require
   [v2.se.jherrlin.music-theory.webapp.events :as events]
   [v2.se.jherrlin.music-theory.webapp.chords :as chords]
   [v2.se.jherrlin.music-theory.webapp.scales :as scales]
   [v2.se.jherrlin.music-theory.webapp.modes :as modes]
   [v2.se.jherrlin.music-theory.webapp.settings :as settings]
   [v2.se.jherrlin.music-theory.webapp.harmonizations :as harmonizations]
   ["semantic-ui-react" :as semantic-ui]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [clojure.string :as str]
   [clojure.set :as set]))


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
