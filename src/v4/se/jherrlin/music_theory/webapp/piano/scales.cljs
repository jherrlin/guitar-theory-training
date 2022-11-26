(ns v4.se.jherrlin.music-theory.webapp.piano.scales
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
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))




(def events-
  [{:n ::scale}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}
   {:n ::as-intervals
    :s (fn [db [n']] (get db n' false))}
   {:n ::nr-of-octavs}
   {:n ::path-params}
   {:n ::query-params}])

(comment
  @(re-frame/subscribe [::as-intervals])
  @re-frame.db/app-db
  )

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))





(defn scales-view []
  (let [scale        @(re-frame/subscribe [::scale])
        as-intervals @(re-frame/subscribe [::as-intervals])
        key-of       @(re-frame/subscribe [:key-of])
        path-params  @(re-frame/subscribe [::path-params])
        query-params @(re-frame/subscribe [::query-params])
        nr-of-octavs @(re-frame/subscribe [::nr-of-octavs])]
    (when (and scale key-of)
      (let [{id          :scale/id
             indexes     :scale/indexes
             intervals   :scale/intervals
             scale-name  :scale/name
             :as m}
            (get @definitions/scales scale)
            tones (utils/tones-by-key-and-intervals
                   (utils/all-tones)
                   key-of
                   intervals)]
        [:div

         ;; Links to keys
         [common/links-to-keys
          key-of
          #(rfe/href :v4.piano/scales (assoc path-params :key-of %) query-params)]

         [:br]

         ;; Links to scales
         [common/links-to-scales
          @definitions/scales
          scale
          #(rfe/href :v4.piano/scales (assoc path-params :scale-name %) query-params)]

         ;; Highlight tones
         [common/highlight-tones tones key-of]

         ;; Chord name
         [common/chord-name key-of m]

         ;; Intervals
         [common/intervals-to-tones intervals tones]

         [:br]

         [:div {:style {:display :flex}}
          (for [index (range 0 nr-of-octavs)]
            ^{:key (str "piano-octav-index-" index)}
            [piano.view/piano
             (if as-intervals
               :interval
               :tone)
             (utils/tones-data-from-key-of-and-intervals
              (utils/all-tones)
              key-of
              intervals)])]]))))


(def routes
  [["/v4/piano/scale/:key-of/:scale-name"
    {:name :v4.piano/scales
     :view [scales-view]
     :controllers
     [{:parameters {:path  [:key-of :scale-name]
                    :query [:as-intervalls :octavs]}
       :start      (fn [{{:keys [key-of scale-name] :as p}    :path
                         {:keys [as-intervalls octavs] :as q} :query}]
                     (js/console.log "path params" p)
                     (js/console.log "query params" q)
                     (re-frame/dispatch [::path-params p])
                     (re-frame/dispatch [::query-params q])
                     (let [octavs (if-not octavs
                                    2
                                    (js/parseInt octavs))
                           key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (re-frame/dispatch [::nr-of-octavs octavs])
                       (re-frame/dispatch [:key-of (keyword key-of)])
                       (re-frame/dispatch [::scale (keyword scale-name)])
                       (re-frame/dispatch [::as-intervals (boolean (seq as-intervalls))])))}]}]])
