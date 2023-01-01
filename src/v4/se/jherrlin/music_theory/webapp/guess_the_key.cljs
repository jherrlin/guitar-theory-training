(ns v4.se.jherrlin.music-theory.webapp.guess-the-key
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v4.se.jherrlin.music-theory.definitions :as definitions]
   [v4.se.jherrlin.music-theory.intervals :as intervals]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]
   [v4.se.jherrlin.music-theory.webapp.piano.common :as common]
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))



(def events-
  [{:n ::chords}
   {:n ::data}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))


(defn guess-the-key-view []
  (let [chords @(re-frame/subscribe [::chords])
        data   @(re-frame/subscribe [::data])]
    [:div
     [:input {:type      "text"
              :value     chords
              :on-change #(re-frame/dispatch [::chords (.. % -target -value)])}]
     [:button
      {:on-click #(re-frame/dispatch [::data
                                      (->> (utils/read-incomming-chord-names chords)
                                           (map (partial utils/chord-by-name @definitions/chords))
                                           (utils/possible-scales)
                                           (utils/posibble-chord-in-harmonization
                                            @definitions/scales
                                            @definitions/chords))])}
      "Find"]

     (when-not (str/blank? chords)
       (for [{scale  :possible-scale/scale
              key-of :possible-scale/key-of
              chords :chords}
             data]
         ^{:key (str scale key-of)}
         [:<>
          [:h2 (str (str/capitalize (name key-of)) " " (name scale))]

          (for [{:keys [incomming-chord possible-chord]} chords]
            [:<>
             [:div {:style {:display "flex"}}
              [:p {:style {:margin-right "2rem"}} (:chord-name possible-chord)]
              [:p {:style {:margin-right "2rem"}} (:harmonization/position possible-chord)]
              [:p (str "Mode: " (name (:harmonization/mode possible-chord)))]]])
          [:hr]]))


     #_[:pre
      (with-out-str (cljs.pprint/pprint data))]]))


(def routes
  ["/v4/guess-the-key"
   {:name :v4/guess-the-key
    :view [guess-the-key-view]
    :controllers
    [{:parameters {:path  params/path-params
                   :query params/query-params}
      :start      (fn [{p :path q :query}]
                    (re-frame/dispatch [:path-params p])
                    (re-frame/dispatch [:query-params q]))}]}])


(re-frame/dispatch [::chords "dm7 g7 cmaj7"])
