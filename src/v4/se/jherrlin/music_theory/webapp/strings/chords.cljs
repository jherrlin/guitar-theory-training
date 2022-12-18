(ns v4.se.jherrlin.music-theory.webapp.strings.chords
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v4.se.jherrlin.music-theory.definitions :as definitions]
   [v4.se.jherrlin.music-theory.intervals :as intervals]
   [v4.se.jherrlin.music-theory.webapp.strings.styled-fretboard :refer [styled-view]]
   [se.jherrlin.utils :as utils-tools]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]
   [v4.se.jherrlin.music-theory.webapp.piano.common :as common]
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))


(defn debug-view []
  [:pre
   (with-out-str (cljs.pprint/pprint @re-frame.db/app-db))])

(defn chords-view []
  (let [key-of            @(re-frame/subscribe [:key-of])
        chord             @(re-frame/subscribe [:chord])
        nr-of-frets       @(re-frame/subscribe [:nr-of-frets])
        path-params       @(re-frame/subscribe [:path-params])
        query-params      @(re-frame/subscribe [:query-params])
        highlighted-tones @(re-frame/subscribe [:highlighted-tones])
        instrument        @(re-frame/subscribe [:instrument])
        debug?            @(re-frame/subscribe [:debug])
        trim?             @(re-frame/subscribe [:trim])
        as-intervals      @(re-frame/subscribe [:as-intervals])
        combined-triads?  @(re-frame/subscribe [:combined-triads])
        interval-to-tone  @(re-frame/subscribe [:interval-to-tone])
        path-name         @(re-frame/subscribe [:path-name])
        as-text           @(re-frame/subscribe [:as-text])]
    [:div
     (when (and chord key-of)
       (let [{chord-id    :chord/id
              indexes     :chord/indexes
              intervals   :chord/intervals
              explanation :chord/explanation
              sufix       :chord/sufix
              text        :chord/text
              :as         m}
             (get @definitions/chords chord)
             index-tones       (utils/index-tones indexes key-of)
             interval-tones    (utils/interval-tones intervals key-of)
             instrument-data   (definitions/tuning instrument)
             instrument-info   (:text instrument-data)
             instrument-tuning (:tuning instrument-data)
             fretboard-strings (utils/fretboard-strings
                                (utils/all-tones)
                                instrument-tuning
                                nr-of-frets)]
         [:div

          [common/links-to-keys
           key-of
           #(rfe/href path-name (assoc path-params :key-of %) query-params)]

          [:br]

          [common/links-to-chords
           @definitions/chords
           chord
           #(rfe/href path-name (assoc path-params :chord %) query-params)]

          ;; Highlight tones
          (when highlighted-tones
            [common/highlight-tones interval-tones key-of])

          ;; Chord name
          [common/chord-name key-of m]

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
           [:a {:style {:margin-right "10px"}
                :href  (rfe/href path-name path-params (assoc query-params :as-intervals (not as-intervals)))}
            [:button
             (str "Show as " (if as-intervals "tones" "intervals"))]]

           [:a {:style {:margin-right "10px"}
                :href (rfe/href path-name path-params (assoc query-params :as-text (not as-text)))}
            [:button
             (str "Show " (if as-text "styled" "as text"))]]

           [:a {:href (rfe/href path-name path-params (assoc query-params :trim (not trim?)))}
            [:button
             (if trim? "Full" "Trim" )]]]


          ;; All chord tones
          [:h3 "All " (if as-intervals "interval" "tone") " positions in the chord"]
          (let [data  (if as-intervals
                        (utils/with-all-intervals
                          (mapv vector interval-tones intervals)
                          fretboard-strings)
                        (utils/with-all-tones interval-tones fretboard-strings))
                data' (if-not trim?
                        data
                        (utils-tools/trim-matrix #(every? nil? (map :out %)) data))
                id'   (str chord-id as-intervals)]
            [common/fretboard-str-with-add-button
             id'
             data'
             #(re-frame/dispatch
               [:notebook/add
                {:id      id'
                 :url     [path-name path-params query-params]
                 :title   (str key-of "" chord)
                 :version :v1
                 :view    (if as-text :text/fretboard :css/fretboard)
                 :data    data}])])

          ;; Chord patterns
          (let [chord-patterns (->> @definitions/chord-patterns
                                    (vals)
                                    (filter (comp #{chord} :fretboard-pattern/name))
                                    (filter (comp #{instrument-tuning} :fretboard-pattern/tuning))
                                    (sort-by :chord/pattern-title))]
            (when (seq chord-patterns)
              [:<>
               [:h3 "Chord patterns"]
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
                    (let [id' (str "chord-pattern-" id "-" as-intervals)]
                      [common/fretboard-str-with-add-button
                       id'
                       data'
                       #(re-frame/dispatch
                         [:notebook/add
                          {:id      id'
                           :url     [path-name path-params query-params]
                           :title   (str "Chord: " key-of "" chord)
                           :version :v1
                           :view    (if as-text :text/fretboard :css/fretboard)
                           :data    data}])])]))]))

          ;; triad patterns
          (let [triad-patterns     (->> @definitions/triad-patterns
                                        vals
                                        (filter (comp #{chord} :fretboard-pattern/name))
                                        (filter (comp #{instrument-tuning} :fretboard-pattern/tuning)))
                grouped-on-strings (->> triad-patterns
                                        (group-by :fretboard-pattern/on-strings)
                                        (vals)
                                        (reverse)
                                        (sort-by (comp #(apply + %) :fretboard-pattern/on-strings first)))
                _                  (def triad-patterns triad-patterns)
                _                  (def grouped-on-strings grouped-on-strings)]
            (when (or (seq triad-patterns) (seq grouped-on-strings))
              [:<>
               [:h3 "Triads"]
               [:a {:href (rfe/href path-name path-params (assoc query-params :combined-triads (not combined-triads?)))}
                [:button
                 (if combined-triads?
                   "Separated patterns" "Combined patterns")]]
               (if combined-triads?
                 (for [triad-groups grouped-on-strings]
                   ^{:key (str "triad-" triad-groups)}
                   (let [combined-triads (->> triad-groups
                                              (map :fretboard-pattern/pattern)
                                              (utils/merge-matrix
                                               nr-of-frets
                                               (fn [pattern]
                                                 (if as-intervals
                                                   (utils/pattern-with-intervals
                                                    key-of
                                                    pattern
                                                    fretboard-strings)
                                                   (utils/pattern-with-tones
                                                    key-of
                                                    pattern
                                                    fretboard-strings)))))]
                     [:div {:style {:margin-top "2em"}}
                      (let [id (->> triad-groups first :fretboard-pattern/id (str "combined-") (str as-intervals))]
                        [common/fretboard-str-with-add-button
                         id
                         combined-triads
                         #(re-frame/dispatch
                           [:notebook/add
                            {:id      id
                             :version :v1
                             :url     [path-name path-params query-params]
                             :title   (str "Triad: " key-of "" chord)
                             :view    (if as-text :text/fretboard :css/fretboard)
                             :data    combined-triads}])])]))

                 (for [{id      :fretboard-pattern/id
                        pattern :fretboard-pattern/pattern}
                       (->> triad-patterns
                            (sort-by
                             (fn [{on-strings :fretboard-pattern/on-strings}]
                               (apply + on-strings))))]
                   ^{:key (-> id name)}
                   (let [data (if as-intervals
                                (utils/pattern-with-intervals
                                 key-of
                                 pattern
                                 fretboard-strings)
                                (utils/pattern-with-tones
                                 key-of
                                 pattern
                                 fretboard-strings))]

                     [:div {:style {:margin-top "2em"}}
                      (let [id' (str id as-intervals)]
                        [common/fretboard-str-with-add-button
                         id'
                         data
                         #(re-frame/dispatch
                           [:notebook/add
                            {:id      id'
                             :version :v1
                             :url     [path-name path-params query-params]
                             :title   (str "Triad: " key-of "" chord)
                             :view    (if as-text :text/fretboard :css/fretboard)
                             :data    data}])])])))]))

          (let [scales-to-chord (utils/scales-to-chord @definitions/scales indexes)]
            (when (seq scales-to-chord)
              [:<>
               [:h3 "Scales to chord"]
               (for [{scale-title :scale/name
                      scale-id    :scale/id}
                     scales-to-chord]
                 ^{:key scale-title}
                 [:div {:style {:margin-right "10px" :display "inline"}}
                  [:a {:href
                       (rfe/href :v4.strings/scale (assoc path-params :scale scale-id) query-params)}
                   [:button scale-title]]])]))]))
     (when debug?
       [debug-view])]))


(def routes
  (let [path-name :v4.strings/chord]
    ["/v4/strings/:instrument/chord/:key-of/:chord"
     {:name path-name
      :view [chords-view]
      :controllers
      [{:parameters {:path  params/path-params
                     :query params/query-params}
        :start      (fn [{p :path q :query}]
                      (re-frame/dispatch [:path-params p])
                      (re-frame/dispatch [:query-params q])
                      (re-frame/dispatch [:path-name path-name]))}]}]))
