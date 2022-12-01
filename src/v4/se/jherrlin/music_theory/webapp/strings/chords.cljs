(ns v4.se.jherrlin.music-theory.webapp.strings.chords
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
        ]
    [:div
     (when (and chord key-of)
       (let [{indexes     :chord/indexes
              intervals   :chord/intervals
              explanation :chord/explanation
              sufix       :chord/sufix
              text        :chord/text
              :as         m}
             (get @definitions/chords chord)
             index-tones       (utils/index-tones indexes key-of)
             interval-tones    (utils/interval-tones intervals key-of)
             fretboard-strings (utils/fretboard-strings
                                (utils/all-tones)
                                definitions/standard-guitar-tuning
                                nr-of-frets)
             tuning-tones      definitions/standard-guitar-tuning]
         [:div

          [common/links-to-keys
           key-of
           #(rfe/href :v4.strings/chord (assoc path-params :key-of %) query-params)]

          [common/links-to-chords
           @definitions/chords
           chord
           #(rfe/href :v4.strings/chord (assoc path-params :chord %) query-params)]

          ;; Highlight tones
         (when highlighted-tones
           [common/highlight-tones interval-tones key-of])

                   ;; Chord name
         [common/chord-name key-of m]

                   ;; Intervals
         (when interval-to-tone
           [common/intervals-to-tones intervals interval-tones])


          ;; All chord tones
          [:p "All notes in chord"]
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
                    :url     [:v4.strings/chord path-params query-params]
                    :title   (str key-of "" chord)
                    :version :v1
                    :view    :text/fretboard
                    :data    data}])}
              "Add"]])]


          ;; Chord patterns
          [:p "Chord patterns"]
          (let [chord-patterns (->> @definitions/chord-patterns
                                    (vals)
                                    (filter (comp #{chord} :fretboard-pattern/name))
                                    (filter (comp #{tuning-tones} :fretboard-pattern/tuning))
                                    (sort-by :chord/pattern-title))]
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
                          :url     [:v4.strings/chord path-params query-params]
                          :title   (str key-of "" chord)
                          :version :v1
                          :view    :text/fretboard
                          :data    data}])}
                     "Add"]]]))))

          ;; triad patterns
          [:p "Triad patterns"]
          (let [triad-patterns     (->> @definitions/triad-patterns
                                        vals
                                        (filter (comp #{chord} :fretboard-pattern/name))
                                        (filter (comp #{tuning-tones} :fretboard-pattern/tuning)))
                grouped-on-strings (->> triad-patterns
                                        (group-by :fretboard-pattern/on-strings)
                                        (vals)
                                        (reverse)
                                        (sort-by (comp #(apply + %) :fretboard-pattern/on-strings first)))]
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
                   [:pre {:style {:overflow-x "auto"
                                  :margin     "0em"}}
                   (utils/fretboard-str
                    (fn [{:keys [out]}] (if (nil? out) "" out))
                    combined-triads)]
                   [:button
                    {:on-click
                     #(re-frame/dispatch
                       [:notebook/add
                        {:id      (cljs.core/random-uuid)
                         :version :v1
                         :url     [:v4.strings/chord path-params query-params]
                         :title   (str key-of "" chord)
                         :view    :text/fretboard
                         :data    combined-triads}])}
              "Add"]]))

              (for [{id      :fretboard-pattern/id
                     pattern :fretboard-pattern/pattern}
                    triad-patterns]
                ^{:key (-> id name)}
                [:div {:style {:margin-top "2em"}}
                 [:pre {:style {:overflow-x "auto"}}
                  (let [data (if as-intervals
                               (utils/pattern-with-intervals
                                key-of
                                pattern
                                fretboard-strings)
                               (utils/pattern-with-tones
                                key-of
                                pattern
                                fretboard-strings))]
                    [:<>
                     [:pre {:style {:overflow-x "auto"
                                    :margin     "0em"}}
                     (utils/fretboard-str
                      (fn [{:keys [out]}] (if (nil? out) "" out))
                      data)]
                     [:button
                      {:on-click
                       #(re-frame/dispatch
                         [:notebook/add
                          {:id      (cljs.core/random-uuid)
                           :version :v1
                           :url     [:v4.strings/chord path-params query-params]
                           :title   (str key-of "" chord)
                           :view    :text/fretboard
                           :data    data}])}
              "Add"]])]])))

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
  [["/v4/strings/:instrument/chord/:key-of/:chord"
    {:name :v4.strings/chord
     :view [chords-view]
     :controllers
     [{:parameters {:path  params/path-params
                    :query params/query-params}
       :start      (fn [{p :path q :query}]
                     (re-frame/dispatch [:path-params p])
                     (re-frame/dispatch [:query-params q]))}]}]])
