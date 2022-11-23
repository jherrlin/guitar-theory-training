(ns v4.se.jherrlin.music-theory.webapp.piano.chords
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
  [{:n ::chord}
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


(defn chords-view []
  (let [chord        @(re-frame/subscribe [::chord])
        as-intervals @(re-frame/subscribe [::as-intervals])
        key-of       @(re-frame/subscribe [:key-of])
        path-params  @(re-frame/subscribe [::path-params])
        query-params @(re-frame/subscribe [::query-params])
        nr-of-octavs @(re-frame/subscribe [::nr-of-octavs])]
    (when (and chord key-of)
      (let [{id          :chord/id
             indexes     :chord/indexes
             intervals   :chord/intervals
             explanation :chord/explanation
             sufix       :chord/sufix
             :as m}
            (get @definitions/chords chord)
            tones (utils/tones-by-key-and-intervals
                   utils/all-tones
                   key-of
                   intervals)]
        [:div

         ;; Links to keys
         [common/links-to-keys
          key-of
          #(rfe/href :v4.piano/chords (assoc path-params :key-of %) query-params)]

         [:br]

         ;; Links to chords
         [common/links-to-chords
          @definitions/chords
          chord
          #(rfe/href :v4.piano/chords (assoc path-params :chord-name %) query-params)]

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
             (utils/tones-by-key-and-intervals-2
              utils/all-tones
              key-of
              intervals)]
            )]]))))

(def routes
  [["/v4/piano/chord/:key-of/:chord-name"
    {:name :v4.piano/chords
     :view [chords-view]
     :controllers
     [{:parameters {:path  [:key-of :chord-name]
                    :query [:as-intervalls :octavs]}
       :start      (fn [{{:keys [key-of chord-name] :as p}    :path
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
                       (re-frame/dispatch [::chord (keyword chord-name)])
                       (re-frame/dispatch [::as-intervals (boolean (seq as-intervalls))])))}]}]])
