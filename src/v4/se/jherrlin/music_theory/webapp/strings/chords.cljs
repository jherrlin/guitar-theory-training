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
  (let [key-of       @(re-frame/subscribe [:key-of])
        chord        @(re-frame/subscribe [:chord])
        nr-of-frets  @(re-frame/subscribe [:nr-of-frets])
        instrument   @(re-frame/subscribe [:instrument])
        debug?       @(re-frame/subscribe [:debug])
        trim?        @(re-frame/subscribe [:trim])
        as-intervals @(re-frame/subscribe [:as-intervals])]
    [:div
     (when (and chord key-of)
       (let [{indexes     :chord/indexes
              intervals   :chord/intervals
              explanation :chord/explanation
              sufix       :chord/sufix
              text        :chord/text}
             (get @definitions/chords chord)
             index-tones       (utils/index-tones indexes key-of)
             interval-tones    (utils/interval-tones intervals key-of)
             fretboard-strings (utils/fretboard-strings
                                (utils/all-tones)
                                definitions/standard-guitar-tuning
                                nr-of-frets)
             tuning-tones      definitions/standard-guitar-tuning]
         (def fretboard-strings fretboard-strings)
         (def interval-tones interval-tones)
         (def intervals intervals)
         (def chord chord)
         (def tuning-tones tuning-tones)
         (def trim? trim?)
         [:div
          [:div "chord view"]

          ;; All chord tones
          [:div
           (let [data  (if as-intervals
                         (utils/with-all-intervals
                           (mapv vector interval-tones intervals)
                           fretboard-strings)
                         (utils/with-all-tones interval-tones fretboard-strings))
                 data' (if-not trim?
                         data
                         (utils-tools/trim-matrix #(every? nil? (map :out %)) data))]
             [:pre {:style {:overflow-x "auto"}}
              (utils/fretboard-str
               (fn [{:keys [out]}] (if (nil? out) "" out))
               data')])]


          ;; Chord patterns
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
                (let [data  (utils/pattern-with-tones
                             key-of
                             pattern
                             fretboard-strings)
                      data' (if-not trim?
                              data
                              (utils-tools/trim-matrix #(every? nil? (map :out %)) data))]
                  [:div
                   [:button
                    {:on-click
                     #(re-frame/dispatch
                       [:notebook/add
                        {:id      (cljs.core/random-uuid)
                         :version :v1
                         :view    :text/fretboard
                         :data    data}])}
                    "add"]
                   [:pre {:style {:overflow-x "auto"}}
                   (utils/fretboard-str
                    (fn [{:keys [out]}] (if (nil? out) "" out))
                    data')]]
                  ))))]))
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
