(ns v2.se.jherrlin.music-theory.webapp.settings
  (:require
   [re-frame.core :as re-frame]
   [v2.se.jherrlin.music-theory.definitions :as definitions]))

(defn settings-view []
  (let [nr-of-frets  @(re-frame/subscribe [:nr-of-frets])
        tuning-name  @(re-frame/subscribe [:tuning-name])
        tuning-tones @(re-frame/subscribe [:tuning-tones])]
    [:div
     [:div
      [:label "Number of frets"]
      [:br]
      [:input
       {:type      "number"
        :value     nr-of-frets
        :on-change #(re-frame/dispatch [:nr-of-frets (-> % .-target .-value)])}]]

     [:div
      [:label "Tuning"]
      [:br]
      [:select
       {:on-change
        #(do
           (re-frame/dispatch [:tuning-name (keyword (.. % -target -value))])
           (re-frame/dispatch [:tuning-tones (if (= :guitar (.. % -target -value))
                                               definitions/standard-guitar-tuning
                                               definitions/standard-ukulele-tuning)]))}
       [:option {:value :guitar} "Guitar"]
       [:option {:value :ukulele} "Ukulele"]]]]))

(def routes
  ["settings"
   {:name      :v2/settings
    :view      [settings-view]}])
