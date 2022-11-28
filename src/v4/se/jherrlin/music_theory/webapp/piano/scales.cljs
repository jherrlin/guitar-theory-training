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
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))




(defn scales-view []
  (let [scale             @(re-frame/subscribe [:scale])
        as-intervals      @(re-frame/subscribe [:as-intervals])
        key-of            @(re-frame/subscribe [:key-of])
        path-params       @(re-frame/subscribe [:path-params])
        query-params      @(re-frame/subscribe [:query-params])
        nr-of-octavs      @(re-frame/subscribe [:nr-of-octavs])
        highlighted-tones @(re-frame/subscribe [:highlighted-tones])
        interval-to-tone @(re-frame/subscribe [:interval-to-tone])]
    (when (and scale key-of)
      (let [{id         :scale/id
             indexes    :scale/indexes
             intervals  :scale/intervals
             scale-name :scale/name
             :as        m}
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
          #(rfe/href :v4.piano/scales (assoc path-params :scale %) query-params)]

         ;; Highlight tones
         (when highlighted-tones
           [common/highlight-tones tones key-of])


         ;; Chord name
         [common/chord-name key-of m]

         ;; Intervals
         (when interval-to-tone
           [common/intervals-to-tones intervals tones])


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
  [["/v4/piano/scale/:key-of/:scale"
    {:name :v4.piano/scales
     :view [scales-view]
     :controllers
     [{:parameters {:path  params/path-params
                    :query params/query-params}
       :start      (fn [{p :path q :query}]
                     (re-frame/dispatch [:path-params p])
                     (re-frame/dispatch [:query-params q]))}]}]])
