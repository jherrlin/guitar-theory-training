(ns v4.se.jherrlin.music-theory.webapp.strings.harmonizations
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






(defn harmonizations-view []
  (let [scale        @(re-frame/subscribe [:scale])
        steps        @(re-frame/subscribe [:steps])
        as-intervals @(re-frame/subscribe [:as-intervals])
        key-of       @(re-frame/subscribe [:key-of])
        instrument   @(re-frame/subscribe [:instrument])
        path-params  @(re-frame/subscribe [:path-params])
        query-params @(re-frame/subscribe [:query-params])
        nr-of-frets  @(re-frame/subscribe [:nr-of-frets])
        trim?        @(re-frame/subscribe [:trim])
        path-name    @(re-frame/subscribe [:path-name])
        as-text      @(re-frame/subscribe [:as-text])]
    (when (and scale key-of)
      (let [{id        :scale/id
             indexes   :scale/indexes
             intervals :scale/intervals
             :as       m}
            (get @definitions/scales scale)
            tones             (utils/tones-by-key-and-intervals
                               (utils/all-tones)
                               key-of
                               intervals)
            instrument-data   (definitions/tuning instrument)
            instrument-info   (:text instrument-data)
            instrument-tuning (:tuning instrument-data)
            fretboard-strings (utils/fretboard-strings
                               (utils/all-tones)
                               instrument-tuning
                               nr-of-frets)
            harmonization'    (utils/gen-harmonization
                               @definitions/scales
                               @definitions/chords
                               key-of
                               scale
                               (condp = steps
                                 :seventh utils/seventh
                                 utils/triad))]
        [:div

         ;; Links to keys
         [:div #_{:style {:display         :flex
                          :justify-content :center}}
          [common/links-to-keys
           key-of
           #(rfe/href :v4.strings/harmonizations (assoc path-params :key-of %) query-params)]]

         [:br]

         [:div #_{:style {:display         :flex
                          :justify-content :center}}
          [:a {:style {:margin-right "10px" :display "inline"}
               :href  (rfe/href :v4.strings/harmonizations (assoc path-params :steps :triad) query-params)}
           [:button
            {:disabled (= :triad steps)}
            "triad"]]

          [:a {:style {:margin-right "10px" :display "inline"}
               :href  (rfe/href :v4.strings/harmonizations (assoc path-params :steps :seventh) query-params)}
           [:button
            {:disabled (= :seventh steps)}
            "seventh"]]


          [:a {:style {:margin-right "10px" :display "inline"}
               :href  (rfe/href :v4.strings/harmonizations (assoc path-params :scale :major) query-params)}
           [:button
            {:disabled (= :major scale)}
            "major"]]

          [:a {:href (rfe/href :v4.strings/harmonizations (assoc path-params :scale :minor) query-params)}
           [:button
            {:disabled (= :minor scale)}
            "minor"]]]


         [:br]


         [:div {:style {:display "flex"}}
          [:a {:style {:margin-right "10px"}
               :href  (rfe/href path-name path-params (assoc query-params :as-intervals (not as-intervals)))}
           [:button
            (str "Show as " (if as-intervals "tones" "intervals"))]]

          [:a {:style {:margin-right "10px"}
               :href  (rfe/href path-name path-params (assoc query-params :as-text (not as-text)))}
           [:button
            (str "Show " (if as-text "styled" "as text"))]]]

         [:br]

         ;; Highlight tones
         (let [interval-tones (utils/interval-tones intervals key-of)]
           (when interval-tones
             [common/highlight-tones interval-tones key-of]))

         [:br]

         [:div #_{:style {:display         :flex
                          :justify-content :center}}
          [:code
           [:pre {:style {:overflow-x "auto"}}
            (->> harmonization' (utils/harmonization-str))]]]


         [:br]

         [:h2 "All tones in harmonization"]

         (let [interval-tones (utils/interval-tones intervals key-of)
               data           (if as-intervals
                                (utils/with-all-intervals
                                  (mapv vector interval-tones intervals)
                                  fretboard-strings)
                                (utils/with-all-tones interval-tones fretboard-strings))
               data'          (if-not trim?
                                data
                                (utils-tools/trim-matrix #(every? nil? (map :out %)) data))
               id'            (str "harmonization-all-tones-" as-intervals)]
           [common/fretboard-str-with-add-button
            id'
            data'
            #(re-frame/dispatch
              [:notebook/add
               {:id      id'
                :url     [path-name path-params query-params]
                :title   (str key-of "")
                :version :v1
                :view    (if as-text :text/fretboard :css/fretboard)
                :data    data}])])

         [:br ]

         [:h2 "Chords in harmonization"]

         (for [{index          :harmonization/index
                chord-id       :chord/id
                interval-tones :chord/interval-tones
                index-tones    :chord/index-tones
                intervals      :chord/intervals
                mode-str       :harmonization/mode-str
                sufix          :chord/sufix
                root           :chord/root-tone
                family-str     :harmonization/family-str
                :as            m}
               harmonization']
           ^{:key (str "strings-harmonization-index-" mode-str)}
           [:div {:style {:display         :flex
                          :flex-direction  :column
                          :justify-content :center}}
            [:div {:style {:display :flex
                                        ;:justify-content :center
                           }}
             [:h3 {:style {:margin-right "2em"}} index]
             [:p {:style {:margin-right "2em"}}
              (str (some-> interval-tones first name str/capitalize) sufix)]
             [:p {:style {:margin-right "2em"}} mode-str]
             [:p {:style {:margin-right "2em"}} family-str]
             [:p {:style {:margin-right "2em"}}
              (->> intervals (str/join ", "))]
             [:p {:style {:margin-right "2em"}}
              (->> (utils/tones-by-key-and-intervals root intervals)
                   (map (comp str/capitalize name))
                   (str/join ", "))]
             [:a {:href (rfe/href :v4.strings/chord (assoc path-params :key-of root :chord chord-id) query-params)}
              [:button
               "Goto"]]]

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
                   :title   (str key-of "")
                   :version :v1
                   :view    (if as-text :text/fretboard :css/fretboard)
                   :data    data}])])

            [:br]
            [:br]])]))))



(def routes
  (let [path-name :v4.strings/harmonizations]
    ["/v4/strings/:instrument/harmonization/:key-of/:scale/:steps"
     {:name path-name
      :view [harmonizations-view]
      :controllers
      [{:parameters {:path  params/path-params
                     :query params/query-params}
        :start      (fn [{p :path q :query}]
                      (re-frame/dispatch [:path-params p])
                      (re-frame/dispatch [:query-params q])
                      (re-frame/dispatch [:path-name path-name]))}]}]))
