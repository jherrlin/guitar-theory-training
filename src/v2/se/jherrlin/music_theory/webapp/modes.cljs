(ns v2.se.jherrlin.music-theory.webapp.modes
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v2.se.jherrlin.music-theory.definitions :as definitions]
   [v2.se.jherrlin.music-theory.intervals :as intervals]
   [v2.se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(def events-
  [{:n ::key-of}
   {:n ::scale}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn mode-view []
  (let [nr-of-frets      @(re-frame/subscribe [:nr-of-frets])
        scale            @(re-frame/subscribe [::scale])
        key-of           @(re-frame/subscribe [::key-of])
        tone-or-interval @(re-frame/subscribe [::tone-or-interval])]
    (when (and scale key-of)
      (let [{intervals :scale/intervals
             indexes   :scale/indexes}
            (get @definitions/scales-atom scale)
            tones ((utils/juxt-indexes-and-intervals indexes intervals)
                   (utils/rotate-until #(% key-of) utils/all-tones))]
        [:<>
         ;; Links to keys
         [:div
          (for [{key-of' :key-of
                 :keys   [url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:key-of   x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x name str/capitalize)})))]
            ^{:key url-name}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v2/mode {:scale scale :key-of key-of'})}
              [:button
               {:disabled (= key-of' key-of)}
               title]]])]

         [:br]

         ;; Links to modes
         [:div
          (for [{scale' :scale
                 :keys  [title]}
                (->> @definitions/modes-atom
                     vals
                     (map (fn [{:mode/keys [scale] :as m}]
                            (merge m (get @definitions/scales-atom scale))))
                     (map (fn [{scale :mode/scale title :scale/title}]
                            {:scale scale
                             :title title}))
                     (set)
                     (sort-by :title))]
            ^{:key (str title "-mode-select")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v2/mode {:scale scale' :key-of key-of})}
              [:button
               {:disabled (= scale' scale)}
               title]]])]

         ;; Title
         [:h2 (str (-> key-of name str/capitalize) " - " (-> scale name str/capitalize))]

         ;; Intervals
         [:pre {:style {:overflow-x "auto"}}
          (->> (map
                (fn [interval index]
                  (str (fformat "%8s" interval) " -> " (-> index name str/capitalize)))
                intervals
                ((utils/juxt-indexes-and-intervals indexes intervals)
                 (utils/rotate-until #(% key-of) definitions/all-tones)))
               (str/join "\n")
               (apply str)
               (str "Interval -> Tone\n"))]

         [:button
          {:on-click
           #(re-frame/dispatch
             [::tone-or-interval
              (if (= tone-or-interval :tone)
                :interval
                :tone)])}
          (str
           "Show as "
           (if (= tone-or-interval :tone)
             "intervals"
             "tones"))]

         ;; All tones in mode
         [:h3 (str "All " (if (= tone-or-interval :tone) "tones" "intervals") " in scale")]
         [:pre {:style {:overflow-x "auto"}}
          (utils/fretboard-str
           (utils/fretboard-strings
            utils/rotate-until
            utils/all-tones
            [:e :b :g :d :a :e]
            nr-of-frets)
           (if (= tone-or-interval :tone)
             (partial
              utils/fretboard-tone-str-chord-f tones)
             (partial
              utils/fretboard-tone-str-chord-f-2
              (mapv vector tones intervals))))]

         ;; Mode patterns
         (let [mode-patterns (->> @definitions/modes-atom
                                  (vals)
                                  (filter (comp #{scale} :mode/scale))
                                  (sort-by :mode/pattern-title))]
           (when (seq mode-patterns)
             [:<>
              [:h3 "Mode patterns"]
              [:div
               (for [{id             :mode/id
                      pattern        :mode/pattern
                      root-on-string :mode/string}
                     mode-patterns]
                 ^{:key (-> id name)}
                 [:div {:style {:margin-top "2em"}}
                  (when root-on-string
                    [:p (str "Root on string: " root-on-string)])
                  [:pre {:style {:overflow-x "auto"}}
                   (utils/fretboard-str
                    (utils/find-pattern
                     definitions/all-tones
                     intervals/intervals-map-by-function
                     (utils/fretboard-strings
                      utils/rotate-until
                      definitions/all-tones
                      [:e :b :g :d :a :e]
                      nr-of-frets)
                     key-of
                     pattern)
                    (if (= tone-or-interval :tone)
                      utils/fretboard-tone-str-pattern-f
                      utils/fretboard-interval-f))]])]]))

         ;; Chords to mode
         [:h3 "Chords to mode"]
         (for [{chord-title :chord/title
                chord-id    :chord/id}
               (let [{scale-indexes :scale/indexes}
                     (get @definitions/scales-atom scale)]
                 (->> @definitions/chords-atom
                      (vals)
                      (sort-by :chord/order)
                      (filter (fn [{:chord/keys [indexes]}]
                                (set/subset? (set indexes) (set scale-indexes))))))]
           ^{:key chord-title}
           [:div {:style {:margin-right "10px" :display "inline"}}
            [:a {:href (rfe/href :v2/chord {:chord-name chord-id :key-of key-of})}
             [:button chord-title]]])]))))

(def routes
  ["mode/:scale/:key-of"
   {:name :v2/mode
    :view [mode-view]
    :controllers
    [{:parameters {:path [:scale :key-of]}
      :start      (fn [{{:keys [scale key-of]} :path}]
                    (let [scale'  (keyword scale)
                          key-of' (keyword key-of)]
                      (js/console.log "Entering mode:" scale key-of)
                      (re-frame/dispatch [::scale scale'])
                      (re-frame/dispatch [::key-of key-of'])))
      :stop       (fn [& params] (js/console.log "Leaving mode"))}]}])
