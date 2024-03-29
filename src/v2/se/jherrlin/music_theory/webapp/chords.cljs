(ns v2.se.jherrlin.music-theory.webapp.chords
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
  [{:n ::chord}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}
   {:n ::combined-triads?
    :s (fn [db [n']] (get db n' true))}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn chords-view []
  (let [nr-of-frets      @(re-frame/subscribe [:nr-of-frets])
        tuning-name      @(re-frame/subscribe [:tuning-name])
        tuning-tones     @(re-frame/subscribe [:tuning-tones])
        chord            @(re-frame/subscribe [::chord])
        key-of           @(re-frame/subscribe [:key-of])
        tone-or-interval @(re-frame/subscribe [::tone-or-interval])
        combined-triads? @(re-frame/subscribe [::combined-triads?])
        {indexes     :chord/indexes
         intervals   :chord/intervals
         explanation :chord/explanation
         sufix       :chord/sufix
         text        :chord/text}
        (get-in @definitions/chords-atom [chord])]
    (when (and chord key-of)
      (let [tones ((utils/juxt-indexes-and-intervals indexes intervals)
                   (utils/rotate-until #(% key-of) utils/all-tones))]
        [:div
         ;; Links to keys
         [:div
          (for [{tone' :tone
                 :keys [url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x name str/capitalize)})))]
            ^{:key url-name}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v3/chord {:key-of tone' :chord-name chord :instrument tuning-name})}
              [:button
               {:disabled (= key-of tone')}
               title]]])]
         [:br]

         ;; Links to chords
         [:div
          (for [{id           :chord/id
                 sufix        :chord/sufix
                 display-text :chord/display-text}
                (->> @definitions/chords-atom
                     vals
                     (sort-by :chord/order))]
            ^{:key (str id sufix "-chord")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v3/chord {:key-of key-of :chord-name id :instrument tuning-name})}
              [:button
               {:disabled (= id chord)}
               (str (or display-text sufix))]]])]

         ;; Highlight tones
         [:div {:style {:margin-top  "1em"
                        :display     "flex"
                        :align-items "center"}}
          (for [{:keys [tone match?]}
                (let [tones-set (set tones)]
                  (->> utils/all-tones
                       (utils/rotate-until #(% key-of))
                       (map (fn [tone]
                              (cond-> {:tone tone}
                                (seq (set/intersection tones-set tone))
                                (assoc :match? true))))))]
            ^{:key (str tone "something")}
            [:div {:style {:width     "4.5em"
                           :font-size "0.9em"}}
             (for [t' (sort-by (fn [x]
                                 (let [x (str x)]
                                   (cond
                                     (and (= (count x) 3) (str/includes? x "#"))
                                     1
                                     (= (count x) 3)
                                     2
                                     :else 0))) tone)]
               ^{:key (str tone t')}
               [:div {:style {:margin-top "0em"}}
                [:div
                 {:style {:color       (if-not match? "grey")
                          :font-weight (if match? "bold")}}
                 (-> t' name str/capitalize)]])])]

         ;; Chord name
         [:div {:style {:margin-top "1em"
                        :height     "100%"
                        :display    "inline-flex"}}
          [:h2 (str (-> key-of name str/capitalize) sufix)]
          (when explanation
            [:p {:style {:margin-left "4em"
                         :margin-top  "0.5em"}}
             (str "(" explanation ")")])]

         (when text
           [:p text])

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

         ;; All tones in chord
         [:h3 "All " (if (= tone-or-interval :tone) "tone" "interval") " positions in the chord"]
         [:pre {:style {:overflow-x "auto"}}
          (utils/fretboard-str
           (utils/fretboard-strings
            utils/rotate-until
            utils/all-tones
            tuning-tones
            nr-of-frets)
           (if (= tone-or-interval :tone)
             (partial
              utils/fretboard-tone-str-chord-f tones)
             (partial
              utils/fretboard-tone-str-chord-f-2
              (mapv vector tones intervals))))]

         ;; Chord patterns
         (let [chord-patterns (->> @definitions/chord-patterns-atom
                                   (vals)
                                   (filter (comp #{chord} :chord-pattern/name))
                                   (filter (comp #{tuning-tones} :chord-pattern/tuning))
                                   (sort-by :chord/pattern-title))]
           (when (seq chord-patterns)
             [:<>
              [:h3 "Chord patterns"]
              [:div
               (for [{id      :chord/pattern-id
                      pattern :chord/pattern}
                     chord-patterns]
                 ^{:key (-> id name)}
                 [:div {:style {:margin-top "2em"}}
                  [:pre {:style {:overflow-x "auto"}}
                   (utils/fretboard-str
                    (utils/find-pattern
                     definitions/all-tones
                     intervals/intervals-map-by-function
                     (utils/fretboard-strings
                      utils/rotate-until
                      definitions/all-tones
                      tuning-tones
                      nr-of-frets)
                     key-of
                     pattern)
                    (if (= tone-or-interval :tone)
                      utils/fretboard-tone-str-pattern-f
                      utils/fretboard-interval-f))]])]]))

         ;; Triads
         (let [triad-patterns (->> @definitions/triad-patterns-atom
                                   (vals)
                                   (filter (comp #{chord} :triad-pattern/name))
                                   (filter (comp #{tuning-tones} :triad-pattern/tuning))
                                   (sort-by :triad-pattern/order))]
           (when (seq triad-patterns)
             [:<>
              [:h3 "Triads"]
              [:button {:on-click #(re-frame/dispatch [::combined-triads? (not combined-triads?)])}
               (if combined-triads?
                 "Separated patterns" "Combined patterns")]
              (if combined-triads?
                [:div
                 (for [x (->> @definitions/triad-patterns-atom
                              vals
                              (filter (comp #{chord} :triad-pattern/name))
                              (group-by :triad-pattern/on-strings)
                              (vals)
                              (reverse)
                              (sort-by (comp #(apply + %) :triad-pattern/on-strings first)))]
                   ^{:key (str "triad-" x)}
                   (let [tuning-tones [:e :b :g :d :a :e]
                         nr-of-frets  16
                         desc         (str "On strings: " (str/join "," (-> x first :triad-pattern/on-strings)))]
                     ^{:key (str "triad-" x)}
                     [:<>
                      [:div {:style {:margin-top "2em"}}
                       ;; [:p desc]
                       [:pre {:style {:overflow-x "auto"}}
                        (utils/fretboard-str
                         (->> x
                              (map :triad-pattern/pattern)
                              (map #(utils/find-pattern
                                     definitions/all-tones
                                     intervals/intervals-map-by-function
                                     (utils/fretboard-strings
                                      utils/rotate-until
                                      definitions/all-tones
                                      tuning-tones
                                      nr-of-frets)
                                     key-of
                                     %))
                              (map (partial apply concat))
                              (apply map merge)
                              (partition-all nr-of-frets)
                              (mapv #(mapv identity %)))
                         (if (= tone-or-interval :tone)
                           utils/fretboard-tone-str-pattern-f
                           utils/fretboard-interval-f))]]]))]
                [:div
                 (for [{id      :triad-pattern/id
                        pattern :triad-pattern/pattern}
                       triad-patterns]
                   ^{:key (-> id name)}
                   [:div {:style {:margin-top "2em"}}
                    [:pre {:style {:overflow-x "auto"}}
                     (utils/fretboard-str
                      (utils/find-pattern
                       definitions/all-tones
                       intervals/intervals-map-by-function
                       (utils/fretboard-strings
                        utils/rotate-until
                        definitions/all-tones
                        tuning-tones
                        nr-of-frets)
                       key-of
                       pattern)
                      (if (= tone-or-interval :tone)
                        utils/fretboard-tone-str-pattern-f
                        utils/fretboard-interval-f))]])])]))

         ;; Scales to chord
         (let [scales-to-chord
               (let [{chord-indexes :chord/indexes}
                     (get @definitions/chords-atom chord)]
                 (->> @definitions/scales-atom
                      (vals)
                      (sort-by :scale/order)
                      (filter (fn [{:scale/keys [indexes]}]
                                (set/subset? (set chord-indexes) (set indexes))))))]
           (when (seq scales-to-chord)
             [:<>
              [:h3 "Scales to chord"]
              (for [{scale-title :scale/title
                     scale-id    :scale/id}
                    scales-to-chord]
                ^{:key scale-title}
                [:div {:style {:margin-right "10px" :display "inline"}}
                 [:a {:href (rfe/href :v3/scale {:scale scale-id :key-of key-of :instrument tuning-name})}
                  [:button scale-title]]])]))]))))

(def routes
  [["/v2/chord/:key-of/:chord-name"
    {:name :v2/chord
     :view [chords-view]
     :controllers
     [{:parameters {:path [:key-of :chord-name]}
       :start      (fn [{{:keys [key-of chord-name]} :path}]
                     (let [key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (js/console.log "Entering chord:" key-of chord-name)
                       (re-frame/dispatch [:push-state
                                           :v3/chord
                                           {:key-of     (keyword key-of)
                                            :chord-name (keyword chord-name)
                                            :instrument  @(re-frame/subscribe [:tuning-name])}])))
       :stop       (fn [& params] (js/console.log "Leaving chords..."))}]}]
   ["/v3/:instrument/chord/:key-of/:chord-name"
    {:name :v3/chord
     :view [chords-view]
     :controllers
     [{:parameters {:path [:key-of :chord-name :instrument]}
       :start      (fn [{{:keys [key-of chord-name instrument]} :path}]
                     (let [key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (js/console.log "Entering chord:" key-of chord-name)
                       (re-frame/dispatch [:key-of (keyword key-of)])
                       (re-frame/dispatch [::chord (keyword chord-name)])
                       (re-frame/dispatch [:tuning-name (keyword instrument)])))
       :stop       (fn [& params] (js/console.log "Leaving chords..."))}]}]])
