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

     [:h1 "Guess the key"]

     [:p "Will try to find what key the chords are in."]
     [:p "The algorithm will try to match the chords against a major or minor scale."]

     [:br]

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
      "Analyse"]

     [:br]
     [:br]
     [:br]

     (when-not (str/blank? chords)
       (for [{scale             :possible-scale/scale
              key-of            :possible-scale/key-of
              chords            :chords
              total-match-score :total-match-score}
             data]
         ^{:key (str scale key-of)}
         [:<>
          [:div
           [:h2
            (str (str/capitalize (name key-of)) " " (name scale) " scale")]

           [:p {:style {:display "flex"}}
            (str "Total score: " total-match-score)]]

          [:table

           [:thead
            [:tr
             [:th "Chord"]
             [:th "Function"]
             [:th "Position"]
             [:th "Tones"]
             [:th "Scores"]]]

           [:tbody
            (for [{:keys [incomming-chord possible-chord]} chords]
              (let [{match-first-chord-root :match-score/first-chord-root
                     match-score-scale      :match-score/scale}
                    incomming-chord

                    {match-score-by-type      :match-score/by-type
                     match-score-by-intervals :match-score/by-intervals
                     harmonization-family     :harmonization/family
                     harmonization-position   :harmonization/position
                     chord-interval-tones     :chord/interval-tones}
                    possible-chord]
                [:<>
                 [:tr
                  [:td (:chord-name possible-chord)]
                  [:td (-> harmonization-family name str/capitalize)]
                  [:td harmonization-position]
                  [:td (->> chord-interval-tones (map (comp str/capitalize name)) (str/join ", "))]
                  [:td (str
                        (when (< 0 match-first-chord-root)
                          (str "First chord root is scale key: " match-first-chord-root ", "))

                        (when (< 0 match-score-scale)
                          (str "Scale intersection: " match-score-scale ", "))

                        (when (< 0 match-score-by-type)
                          (str "Types: " match-score-by-type ", "))

                        (when (< 0 match-score-by-intervals)
                          (str "Intervals: " match-score-by-intervals)))]]]))]]
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

(let [chords "Dm7 G7 Cmaj7"]
  (re-frame/dispatch [::chords chords])
  (re-frame/dispatch [::data
                    (->> (utils/read-incomming-chord-names chords)
                         (map (partial utils/chord-by-name @definitions/chords))
                         (utils/possible-scales)
                         (utils/posibble-chord-in-harmonization
                          @definitions/scales
                          @definitions/chords))]))
