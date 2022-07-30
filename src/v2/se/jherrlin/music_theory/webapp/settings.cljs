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
       {:value tuning-name
        :on-change
        #(do
           (re-frame/dispatch [:tuning-name (keyword (.. % -target -value))])
           (re-frame/dispatch [:tuning-tones (if (= :guitar (.. % -target -value))
                                               definitions/standard-guitar-tuning
                                               definitions/standard-ukulele-tuning)])
           (re-frame/dispatch [:push-state
                               :v3/settings
                               {:instrumnt (keyword (.. % -target -value))}]))}
       [:option {:value :guitar} "Guitar"]
       [:option {:value :ukulele} "Ukulele"]]]]))

(def routes
  [["/v2/settings"
    {:name :v2/settings
     :view [settings-view]
     :controllers
     [{:start (fn [_]
                (re-frame/dispatch [:push-state
                                    :v3/settings
                                    {:instrumnt @(re-frame/subscribe [:tuning-name])}]))
       :stop  (fn [& params] (js/console.log "Leaving settings v2"))}]}]
   ["/v3/:instrumnt/settings"
    {:name :v3/settings
     :view [settings-view]
     :controllers
     [{:parameters {:path [:instrumnt]}
       :start      (fn [{{:keys [instrumnt]} :path}]
                     (re-frame/dispatch [:tuning-name (keyword instrumnt)]))
       :stop       (fn [& params] (js/console.log "Leaving settings v3"))}]}]])
