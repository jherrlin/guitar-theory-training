(ns v4.se.jherrlin.music-theory.webapp.table
  (:require
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [v4.se.jherrlin.music-theory.definitions :as definitions]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.piano.common :as common]
   [v4.se.jherrlin.music-theory.webapp.params :as params]))


(defn table-view []
  (let [key-of       @(re-frame/subscribe [:key-of])
        path-name    @(re-frame/subscribe [:path-name])
        path-params  @(re-frame/subscribe [:path-params])
        query-params @(re-frame/subscribe [:query-params])
        debug?       @(re-frame/subscribe [:debug])]
    (when key-of
      [:<>

       ;; Links to keys
       [common/links-to-keys
        key-of
        #(rfe/href path-name (assoc path-params :key-of %) query-params)]

       [:<>
        [:h3 "Chords"]
        [:table
         [:thead
          [:tr
           [:th "Sufix"]
           [:th "Intervals"]
           [:th "Tones"]
           [:th "Description"]
           [:th "Types"]]]
         [:tbody
          (for [{chord-id    :chord/id
                 intervals   :chord/intervals
                 indexes     :chord/indexes
                 sufix       :chord/sufix
                 explanation :chord/explanation
                 types       :chord/types}
                (->> @definitions/chords
                     (vals)
                     (sort-by :chord/id))]
            ^{:key (str "chord-list-" chord-id)}
            [:tr
             [:td sufix]
             [:td
              [:a
               {:href
                (rfe/href
                 :v4.strings/chord
                 (assoc path-params :instrument :guitar :chord chord-id)
                 query-params)}
               (str/join ", " intervals)]]
             [:td
              (->> (utils/tones-by-key-and-intervals key-of intervals)
                   (map (comp str/capitalize name))
                   (str/join ", "))]
             [:td (str/capitalize explanation)]
             [:td (->> types sort (map (comp str/capitalize name)) (str/join ", "))]])]]]

       [:<>
        [:h3 "Scales"]
        [:table
         [:thead
          [:tr
           [:th "Name"]
           [:th "Intervals"]
           [:th "Tones"]]]
         [:tbody
          (for [{scale-id    :scale/id
                 intervals   :scale/intervals
                 name'       :scale/name
                 indexes     :scale/indexes
                 sufix       :scale/sufix
                 explanation :scale/explanation}
                (->> @definitions/scales
                     (vals)
                     (sort-by :scale/id))]
            ^{:key (str "scale-list-" scale-id)}
            [:tr
             [:td name']
             [:td
              [:a
               {:href
                (rfe/href
                 :v4.strings/scale
                 (assoc path-params :instrument :guitar :scale scale-id)
                 query-params)}
               (str/join ", " intervals)]]
             [:td
              (->> (utils/tones-by-key-and-intervals key-of intervals)
                   (map (comp str/capitalize name))
                   (str/join ", "))]])]]]])))


(def routes
  (let [path-name :v4/table]
    ["/v4/table/:key-of"
     {:name path-name
      :view [table-view]
      :controllers
      [{:parameters {:path  params/path-params
                     :query params/query-params}
        :start      (fn [{p :path q :query}]
                      (re-frame/dispatch [:path-params p])
                      (re-frame/dispatch [:query-params q])
                      (re-frame/dispatch [:path-name path-name]))}]}]))
