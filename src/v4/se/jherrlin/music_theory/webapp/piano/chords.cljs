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
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))





(defn chords-view []
  (let [chord             @(re-frame/subscribe [:chord])
        as-intervals      @(re-frame/subscribe [:as-intervals])
        key-of            @(re-frame/subscribe [:key-of])
        path-params       @(re-frame/subscribe [:path-params])
        query-params      @(re-frame/subscribe [:query-params])
        nr-of-octavs      @(re-frame/subscribe [:nr-of-octavs])
        highlighted-tones @(re-frame/subscribe [:highlighted-tones])
        interval-to-tone  @(re-frame/subscribe [:interval-to-tone])]
    (when (and chord key-of)
      (let [{id          :chord/id
             indexes     :chord/indexes
             intervals   :chord/intervals
             explanation :chord/explanation
             sufix       :chord/sufix
             :as         m}
            (get @definitions/chords chord)
            tones (utils/tones-by-key-and-intervals
                   (utils/all-tones)
                   key-of
                   intervals)]
        [:div

         ;; Links to keys
         [common/links-to-keys
          key-of
          #(rfe/href :v4.piano/chord (assoc path-params :key-of %) query-params)]

         [:br]

         ;; Links to chords
         [common/links-to-chords
          @definitions/chords
          chord
          #(rfe/href :v4.piano/chord (assoc path-params :chord %) query-params)]

         ;; Highlight tones
         (when highlighted-tones
           [common/highlight-tones tones key-of])

         ;; Chord name
         [common/chord-name key-of m]

         ;; Intervals
         (when interval-to-tone
           [common/intervals-to-tones intervals tones])


         [:br]

         [:button
          {:on-click
           #(re-frame/dispatch
             [:notebook/add
              {:id      (cljs.core/random-uuid)
               :version :v1
               :url     [:v4.piano/chord path-params query-params]
               :title   (str (-> key-of name str/capitalize) sufix)
               :view    :css/piano
               :data    {:as-intervals   as-intervals
                         :index-tones    (utils/index-tones indexes key-of)
                         :interval-tones (utils/interval-tones intervals key-of)
                         :intervals      intervals
                         :key-of         key-of
                         :nr-of-octavs   nr-of-octavs
                         :title          (str (-> key-of name str/capitalize) sufix)
                         :text           "Chord"}}])}
             "add"]

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
  [["/v4/piano/chord/:key-of/:chord"
    {:name :v4.piano/chord
     :view [chords-view]
     :controllers
     [{:parameters {:path  params/path-params
                    :query params/query-params}
       :start      (fn [{p :path q :query}]
                     (re-frame/dispatch [:path-params p])
                     (re-frame/dispatch [:query-params q]))}]}]])
