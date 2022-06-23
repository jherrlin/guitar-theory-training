(ns se.jherrlin.music-theory.webapp.main
  (:require
   [se.jherrlin.music-theory]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]))


(defn main-component []
  [:div "hej hopp"])

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (rd/render [main-component]
             (.getElementById js/document "app")))

(defn init []
  (println "starting...")
  (mount-root)
  )
