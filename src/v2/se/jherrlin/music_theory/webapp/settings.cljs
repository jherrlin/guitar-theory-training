(ns v2.se.jherrlin.music-theory.webapp.settings
  (:require
   [re-frame.core :as re-frame]))

(defn settings-view []
  (let [nr-of-frets @(re-frame/subscribe [:nr-of-frets])]
    [:div
     [:label "Number of frets"]
     [:br]
     [:input
      {:type "number"
       :value nr-of-frets
       :on-change #(re-frame/dispatch [:nr-of-frets (-> % .-target .-value)])}]]))

(def routes
  ["settings"
   {:name      :v2/settings
    :view      [settings-view]}])
