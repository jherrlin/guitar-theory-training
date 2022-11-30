(ns v4.se.jherrlin.music-theory.webapp.strings.scales
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v4.se.jherrlin.music-theory.definitions :as definitions]
   [v4.se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.utils :as utils-tools]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]
   [v4.se.jherrlin.music-theory.webapp.piano.common :as common]
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))



(defn debug-view
  ([]
   (debug-view @re-frame.db/app-db))
  ([x]
   [:pre
    (with-out-str (cljs.pprint/pprint x))]))


(defn scales-view []
  (let [key-of       @(re-frame/subscribe [:key-of])
        scale        @(re-frame/subscribe [:scale])
        nr-of-frets  @(re-frame/subscribe [:nr-of-frets])
        path-params  @(re-frame/subscribe [:path-params])
        query-params @(re-frame/subscribe [:query-params])
        instrument   @(re-frame/subscribe [:instrument])
        debug?       @(re-frame/subscribe [:debug])
        trim?        @(re-frame/subscribe [:trim])
        as-intervals @(re-frame/subscribe [:as-intervals])]
    [:div
     (when (and scale key-of)
       (let [{id         :scale/id
              intervals  :scale/intervals
              indexes    :scale/indexes
              scale-name :scale/name
              :as        scale'} (get @definitions/scales scale)
             index-tones         (utils/index-tones indexes key-of)
             interval-tones      (utils/interval-tones intervals key-of)
             fretboard-strings   (utils/fretboard-strings
                                  (utils/all-tones)
                                  definitions/standard-guitar-tuning
                                  nr-of-frets)
             tuning-tones        definitions/standard-guitar-tuning]
         [:<>

          ;; All tones in scale
          [:div
           (let [data  (if as-intervals
                         (utils/with-all-intervals
                           (mapv vector interval-tones intervals)
                           fretboard-strings)
                         (utils/with-all-tones interval-tones fretboard-strings))
                 data' (if-not trim?
                         data
                         (utils-tools/trim-matrix #(every? nil? (map :out %)) data))]
             [:<>
              [:pre {:style {:overflow-x "auto"}}
               (utils/fretboard-str
                (fn [{:keys [out]}] (if (nil? out) "" out))
                data')]
              [:button
               {:on-click
                #(re-frame/dispatch
                  [:notebook/add
                   {:id      (cljs.core/random-uuid)
                    :version :v1
                    :url     [:v4.strings/scale path-params query-params]
                    :title   (str key-of " " scale)
                    :view    :text/fretboard
                    :data    data}])}
               "Add"]])]

          ;; Scale patterns
          [:p "Patterns"]

          (let [chord-patterns (->> (merge @definitions/mode-patterns @definitions/scale-patterns)
                                    (vals)
                                    (filter (comp #{scale} :fretboard-pattern/scale))
                                    (filter (comp #{tuning-tones} :fretboard-pattern/tuning)))]
            (when (seq chord-patterns)
              (for [{id      :fretboard-pattern/id
                     pattern :fretboard-pattern/pattern}
                    chord-patterns]
                ^{:key (str "chord-pattern-" id)}
                (let [data  (if as-intervals
                              (utils/pattern-with-intervals
                               key-of
                               pattern
                               fretboard-strings)
                              (utils/pattern-with-tones
                               key-of
                               pattern
                               fretboard-strings))
                      data' (if-not trim?
                              data
                              (utils-tools/trim-matrix #(every? nil? (map :out %)) data))]
                  [:div {:style {:display    :flex
                                 :margin-top "2em"}}
                   [:pre {:style {:overflow-x "auto"
                                  :margin     "0em"}}
                    (utils/fretboard-str
                     (fn [{:keys [out]}] (if (nil? out) "" out))
                     data')]
                   [:div {:style {:display         :flex
                                  :flex-direction  :column
                                  :justify-content :center}}
                    [:button
                     {:style {:background-color "white"
                              :border           "none"}
                      :on-click
                      #(re-frame/dispatch
                        [:notebook/add
                         {:id      (cljs.core/random-uuid)
                          :version :v1
                          :url     [:v4.strings/scale path-params query-params]
                          :title   (str key-of " " scale)
                          :view    :text/fretboard
                          :data    data}])}
                     "Add"]]]))))

          ;; Chords to scale
          [:p "Chords to scale"]
          ;; (def indexes indexes)
          (let [chords-to-scale (utils/chords-to-scale @definitions/chords indexes)]
            (when (seq chords-to-scale)
              [:<>
               [:h3 "Scales to chord"]
               [:p "TODO fix link"]
               (for [{chord-name :chord/name
                      chord-id   :chord/id}
                     chords-to-scale]
                 ^{:key chord-name}
                 [:div {:style {:margin-right "10px" :display "inline"}}
                  [:a {:href
                       #(js/console.log "TODO")
                       #_ (rfe/href :v3/scale {:scale scale-id :key-of key-of :instrument tuning-name})}
                   [:button chord-name]]])]))

          (when debug?
            (debug-view scale'))

          ]

         )
       )
     ]
    )
  )










(def routes
  [["/v4/strings/:instrument/scale/:key-of/:scale"
    {:name :v4.strings/scale
     :view [scales-view]
     :controllers
     [{:parameters {:path  params/path-params
                    :query params/query-params}
       :start      (fn [{p :path q :query}]
                     (re-frame/dispatch [:path-params p])
                     (re-frame/dispatch [:query-params q]))}]}]])
