(ns v4.se.jherrlin.music-theory.webapp.piano.harmonizations
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
   [v4.se.jherrlin.music-theory.webapp.params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))






(def events-
  [{:n ::scale}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}
   {:n ::as-intervals
    :s (fn [db [n']] (get db n' false))}
   {:n ::nr-of-octavs}
   {:n ::steps}])

(comment
  @(re-frame/subscribe [::as-intervals])
  @re-frame.db/app-db
  )

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))


(defn harmonizations-view []
  (let [scale        @(re-frame/subscribe [::scale])
        steps        @(re-frame/subscribe [::steps])
        as-intervals @(re-frame/subscribe [::as-intervals])
        key-of       @(re-frame/subscribe [:key-of])
        path-params  @(re-frame/subscribe [:path-params])
        query-params @(re-frame/subscribe [:query-params])
        nr-of-octavs @(re-frame/subscribe [::nr-of-octavs])]
    (when (and scale key-of)
      (let [{id        :scale/id
             indexes   :scale/indexes
             intervals :scale/intervals
             :as       m}
            (get @definitions/scales scale)
            tones (utils/tones-by-key-and-intervals
                   (utils/all-tones)
                   key-of
                   intervals)]
        [:div

         ;; Links to keys
         [:div {:style {:display         :flex
                        :justify-content :center}}
          [common/links-to-keys
           key-of
           #(rfe/href :v4.piano/harmonizations (assoc path-params :key-of %) query-params)]]

         [:a {:href (rfe/href :v4.piano/harmonizations (assoc path-params :steps :triad) query-params)}
          [:button
           #_{:disabled (= key-of tone')}
           "triad"]]

         [:a {:href (rfe/href :v4.piano/harmonizations (assoc path-params :steps :seventh) query-params)}
          [:button
           #_{:disabled (= key-of tone')}
           "seventh"]]


         [:a {:href (rfe/href :v4.piano/harmonizations (assoc path-params :scale :major) query-params)}
          [:button
           #_{:disabled (= key-of tone')}
           "major"]]

         [:a {:href (rfe/href :v4.piano/harmonizations (assoc path-params :scale :minor) query-params)}
          [:button
           #_{:disabled (= key-of tone')}
           "minor"]]


         [:br]

         (for [{index          :harmonization/index
                interval-tones :chord/interval-tones
                index-tones    :chord/index-tones
                intervals      :chord/intervals
                mode-str       :harmonization/mode-str
                sufix          :chord/sufix
                family-str     :harmonization/family-str
                :as            m}
               (utils/gen-harmonization
                @definitions/scales
                @definitions/chords
                key-of
                scale
                (condp = steps
                  :seventh utils/seventh
                  utils/triad))]
           ^{:key (str "piano-harmonization-index-" mode-str)}
           [:div {:style {:display         :flex
                          :flex-direction  :column
                          :justify-content :center}}
            [:div {:style {:display         :flex
                           :justify-content :center}}
             [:h3 {:style {:margin-right "2em"}} index]
             [:p {:style {:margin-right "2em"}}
              (str (-> interval-tones first name str/capitalize) sufix)]
             [:p {:style {:margin-right "2em"}} mode-str]
             [:p {:style {:margin-right "2em"}} family-str]
             ]
            [:div {:style {:display         :flex
                           :justify-content :center}}

             (for [index (range 0 nr-of-octavs)]
               ^{:key (str "piano-octav-index-" index)}
               [piano.view/piano
                (if as-intervals
                  :interval
                  :tone)
                (mapv
                 (fn [interval interval-tone]
                   {:interval interval :interval-tone interval-tone})
                 intervals
                 interval-tones)])]
            [:br]
            [:br]])]))))

(def routes
  [["/v4/piano/harmonization/:key-of/:scale/:steps"
    {:name :v4.piano/harmonizations
     :view [harmonizations-view]
     :controllers
     [{:parameters {:path  [:key-of :scale :steps]
                    :query [:as-intervalls :octavs]}
       :start      (fn [{{:keys [key-of scale steps] :as p}   :path
                         {:keys [as-intervalls octavs] :as q} :query}]
                     (re-frame/dispatch [:path-params p])
                     (re-frame/dispatch [:query-params q])
                     (let [octavs (if-not octavs
                                    2
                                    (js/parseInt octavs))
                           key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (re-frame/dispatch [::nr-of-octavs octavs])
                       (re-frame/dispatch [:key-of (keyword key-of)])
                       (re-frame/dispatch [::scale (keyword scale)])
                       (re-frame/dispatch [::steps (keyword steps)])
                       (re-frame/dispatch [::as-intervals (= "t" as-intervalls)])))}]}]])
