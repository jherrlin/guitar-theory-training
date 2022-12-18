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




(defn scales-view []
  (let [key-of            @(re-frame/subscribe [:key-of])
        scale             @(re-frame/subscribe [:scale])
        nr-of-frets       @(re-frame/subscribe [:nr-of-frets])
        path-params       @(re-frame/subscribe [:path-params])
        query-params      @(re-frame/subscribe [:query-params])
        instrument        @(re-frame/subscribe [:instrument])
        debug?            @(re-frame/subscribe [:debug])
        trim?             @(re-frame/subscribe [:trim])
        as-intervals      @(re-frame/subscribe [:as-intervals])
        highlighted-tones @(re-frame/subscribe [:highlighted-tones])
        interval-to-tone  @(re-frame/subscribe [:interval-to-tone])
        path-name         @(re-frame/subscribe [:path-name])
        as-text           @(re-frame/subscribe [:as-text])]
    [:div
     (when (and scale key-of)
       (let [{scale-id   :scale/id
              intervals  :scale/intervals
              indexes    :scale/indexes
              scale-name :scale/name
              :as        scale'} (get @definitions/scales scale)
             index-tones         (utils/index-tones indexes key-of)
             interval-tones      (utils/interval-tones intervals key-of)
             instrument-data     (definitions/tuning instrument)
             instrument-info     (:text instrument-data)
             instrument-tuning   (:tuning instrument-data)
             fretboard-strings   (utils/fretboard-strings
                                  (utils/all-tones)
                                  instrument-tuning
                                  nr-of-frets)]
         [:<>
          ;; Links to keys
          [common/links-to-keys
           key-of
           #(rfe/href path-name (assoc path-params :key-of %) query-params)]

          [:br]

          ;; Links to scales
          [common/links-to-scales
           @definitions/scales
           scale
           #(rfe/href path-name (assoc path-params :scale %) query-params)]

          ;; Highlight tones
          (when highlighted-tones
            [common/highlight-tones interval-tones key-of])


          ;; Scale name
          (let [{scale-name :scale/name}
                (get @definitions/scales scale)]
            [:div {:style {:margin-top "1em"}}
             [:h2 (str (-> key-of name str/capitalize) " - " (-> scale-name str/capitalize))]])

          ;; Instrument tuing
          [:div
           [:p
            (str
             "Instrument tuning (lowest string first). "
             (when instrument-info
               instrument-info)
             ": "
             (->> instrument-tuning reverse (map (comp str/capitalize name)) (str/join " ")))]]


          ;; Intervals
          (when interval-to-tone
            [common/intervals-to-tones intervals interval-tones])

          ;; Buttons
          [:div {:style {:display "flex"}}
           [:a {:href (rfe/href path-name path-params (assoc query-params :as-intervals (not as-intervals)))}
            [:button
             (str "Show as " (if as-intervals "tones" "intervals"))]]

           [:a {:href (rfe/href path-name path-params (assoc query-params :as-text (not as-text)))}
            [:button
             (str "Show " (if as-text "styled" "as text"))]]

           [:a {:href (rfe/href path-name path-params (assoc query-params :trim (not trim?)))}
            [:button
             (if trim? "Full" "Trim" )]]]

          [:br]
          [:br]

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
             (let [id (str scale-id "-all-tones-" as-intervals)]
               [common/fretboard-str-with-add-button
                id
                data'
                #(re-frame/dispatch
                  [:notebook/add
                   {:id      id
                    :version :v1
                    :url     [path-name path-params query-params]
                    :title   (str "Scale: " key-of " " scale)
                    :view    (if as-text :text/fretboard :css/fretboard)
                    :data    data}])]))]

          ;; Scale patterns
          (let [chord-patterns (->> (merge @definitions/mode-patterns @definitions/scale-patterns)
                                    (vals)
                                    (filter (comp #{scale} :fretboard-pattern/scale))
                                    (filter (comp #{instrument-tuning} :fretboard-pattern/tuning))
                                    (sort-by
                                     (fn [{on-strings :fretboard-pattern/on-strings}]
                                       (apply + on-strings))
                                     #(compare %2 %1)))]
            (when (seq chord-patterns)
              [:<>
               [:h3 "Patterns"]
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
                   [:div {:style {:margin-top "2em"}}
                    (let [id' (str id as-intervals)]
                      [common/fretboard-str-with-add-button
                       id'
                       data'
                       #(re-frame/dispatch
                         [:notebook/add
                          {:id      id'
                           :version :v1
                           :url     [path-name path-params query-params]
                           :title   (str "Scale: " key-of " " scale)
                           :view    (if as-text :text/fretboard :css/fretboard)
                           :data    data}])])]))]))

          ;; Chords to scale
          (let [chords-to-scale (utils/chords-to-scale @definitions/chords indexes)]
            (when (seq chords-to-scale)
              [:<>
               [:h3 "Scales to chord"]
               (for [{chord-name :chord/name
                      chord-id   :chord/id}
                     chords-to-scale]
                 ^{:key chord-name}
                 [:div {:style {:margin-right "10px" :display "inline"}}
                  [:a {:href
                       (rfe/href :v4.strings/chord (assoc path-params :chord chord-id) query-params)}
                   [:button chord-name]]])]))

          (when debug?
            (common/debug-view scale'))]))]))


(def routes
  (let [path-name :v4.strings/scale]
    ["/v4/strings/:instrument/scale/:key-of/:scale"
     {:name path-name
      :view [scales-view]
      :controllers
      [{:parameters {:path  params/path-params
                     :query params/query-params}
        :start      (fn [{p :path q :query}]
                      (re-frame/dispatch [:path-params p])
                      (re-frame/dispatch [:query-params q])
                      (re-frame/dispatch [:path-name path-name]))}]}]))
