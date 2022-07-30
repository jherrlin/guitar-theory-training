(ns v2.se.jherrlin.music-theory.webapp.neck
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v2.se.jherrlin.music-theory.definitions :as definitions]
   [v2.se.jherrlin.music-theory.intervals :as intervals]
   [v2.se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))


(def neck-view-events-
  [{:n ::neck-view-all
    :s (fn [db [kw]] (get db kw true))}
   {:n ::neck-view-fulls
    :s (fn [db [kw]] (get db kw true))}
   {:n ::neck-view-halfs
    :s (fn [db [kw]] (get db kw true))}])

(doseq [{:keys [n s e]} neck-view-events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn neck-view []
  (let [tuning-name  @(re-frame/subscribe [:tuning-name])
        nr-of-frets  @(re-frame/subscribe [:nr-of-frets])
        all          @(re-frame/subscribe [::neck-view-all])
        fulls        @(re-frame/subscribe [::neck-view-fulls])
        halfs        @(re-frame/subscribe [::neck-view-halfs])]
    [:div
     [:h2 "Neck with all tones"]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-all (not all)])} (if all "Hide" "Show")]
     (when all
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (utils/fretboard-str
          (utils/fretboard-strings
           utils/rotate-until
           utils/all-tones
           (if (= :guitar tuning-name)
             definitions/standard-guitar-tuning
             definitions/standard-ukulele-tuning)
           nr-of-frets)
          (partial
           utils/fretboard-tone-str-chord-f (->> definitions/all-tones
                                                 (map #(utils/sharp-or-flat % "#")))))]])
     [:br]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-fulls (not fulls)])} (if fulls "Hide" "Show")]
     (when fulls
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (utils/fretboard-str
          (utils/fretboard-strings
           utils/rotate-until
           utils/all-tones
           (if (= :guitar tuning-name)
             definitions/standard-guitar-tuning
             definitions/standard-ukulele-tuning)
           nr-of-frets)
          (partial
           utils/fretboard-tone-str-chord-f (->> music-theory/tones
                                                 (filter (comp not #(str/includes? % "#") name)))))]])
     [:br]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-halfs (not halfs)])} (if halfs "Hide" "Show")]
     (when halfs
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (utils/fretboard-str
          (utils/fretboard-strings
           utils/rotate-until
           utils/all-tones
           (if (= :guitar tuning-name)
             definitions/standard-guitar-tuning
             definitions/standard-ukulele-tuning)
           nr-of-frets)
          (partial
           utils/fretboard-tone-str-chord-f (->> music-theory/tones
                                                 (filter (comp not #(= % 1)count name)))))]])]))

(def routes
  ["/v3/:instrument/neck"
   {:name :v3/neck
    :view [neck-view]
    :controllers
    [{:parameters {:path [:instrument]}
      :start      (fn [{{:keys [instrument]} :path}]
                    (let [instrument' (keyword instrument)]
                      (js/console.log "Entering neck v3:" instrument')
                      (re-frame/dispatch [:tuning-name instrument'])))
      :stop       (fn [& params] (js/console.log "Leaving neck v3"))}]}])
