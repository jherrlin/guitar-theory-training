(ns v2.se.jherrlin.music-theory.webapp.scales
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
  [{:n ::scale}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn scales-view []
  (let [tuning-name      @(re-frame/subscribe [:tuning-name])
        tuning-tones     @(re-frame/subscribe [:tuning-tones])
        nr-of-frets      @(re-frame/subscribe [:nr-of-frets])
        scale            @(re-frame/subscribe [::scale])
        key-of           @(re-frame/subscribe [:key-of])
        tone-or-interval @(re-frame/subscribe [::tone-or-interval])]
    (when (and scale key-of)
      (let [{intervals :scale/intervals
             indexes   :scale/indexes}
            (get @definitions/scales-atom scale)
            tones ((utils/juxt-indexes-and-intervals indexes intervals)
                   (utils/rotate-until #(% key-of) utils/all-tones))]
        [:div

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
             [:a {:href (rfe/href :v3/scale {:scale scale :key-of key-of' :instrument tuning-name})}
              [:button
               {:disabled (= key-of' key-of)}
               title]]])]
         [:br]

         ;; Links to scales
         [:div
          (for [{title :scale/title
                 id    :scale/id}
                (->> @definitions/scales-atom
                     vals
                     (sort-by :scale/order))]
            ^{:key (str title "-scale")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v3/scale {:scale id :key-of key-of :instrument tuning-name})}
              [:button
               {:disabled (= scale id)}
               title]]])]

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
            ^{:key (str tone "somekey")}
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

         ;; Scale name
         (let [{title :scale/title}
               (get @definitions/scales-atom scale)]
           [:div {:style {:margin-top "1em"}}
            [:h2 (str (-> key-of name str/capitalize) " - " (-> title str/capitalize))]])
         [:br]

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

         ;; Scale on fretboard
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

         [:code
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
               (mapv vector tones intervals))))]]

         (let [scale-patterns
               (->> @definitions/scale-patterns-atom
                    (vals)
                    (filter (comp #{scale} :scale-pattern/scale))
                    (filter (comp #{tuning-tones} :scale-pattern/tuning))
                    (sort-by :scale-pattern/order))]
           (when (seq scale-patterns)
             [:<>
              [:h3 "Patterns"]
              [:div
               (for [{id      :scale-pattern/id
                      pattern :scale-pattern/pattern}
                     scale-patterns]
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

         ;; Chords to scale
         [:h3 "Chords to scale"]
         (for [{chord-title :chord/title
                chord-id    :chord/id}
               (let [{scale-indexes :scale/indexes}
                     (get @definitions/scales-atom scale)]
                 (->> @definitions/chords-atom
                      (vals)
                      (sort-by :chord/order)
                      (filter (fn [{:chord/keys [indexes]}]
                                (set/subset? (set indexes) (set scale-indexes))))))]
           ^{:key (str chord-title)}
           [:div {:style {:margin-right "10px" :display "inline"}}
            [:a {:href (rfe/href :v3/chord {:chord-name chord-id :key-of key-of :instrument tuning-name})}
             [:button chord-title]]])]))))

(def routes
  [["/v2/scale/:scale/:key-of"
    {:name :v2/scale
     :view [scales-view]
     :controllers
     [{:parameters {:path [:scale :key-of]}
       :start      (fn [{{:keys [scale key-of]} :path}]
                     (let [scale'  (keyword scale)
                           key-of' (keyword key-of)]
                       (js/console.log "Entering scale v2:" scale key-of)
                       (re-frame/dispatch [:push-state
                                           :v3/scale
                                           {:key-of    key-of'
                                            :scale     scale'
                                            :instrument @(re-frame/subscribe [:tuning-name])}])))
       :stop       (fn [& params] (js/console.log "Leaving scale v2"))}]}]
   ["/v3/:instrument/scale/:scale/:key-of"
    {:name :v3/scale
     :view [scales-view]
     :controllers
     [{:parameters {:path [:scale :key-of :instrument]}
       :start      (fn [{{:keys [scale key-of instrument]} :path}]
                     (let [scale'  (keyword scale)
                           key-of' (keyword key-of)]
                       (js/console.log "Entering scale v3:" scale key-of)
                       (re-frame/dispatch [::scale scale'])
                       (re-frame/dispatch [:key-of key-of'])
                       (re-frame/dispatch [:tuning-name (keyword instrument)])))
       :stop       (fn [& params] (js/console.log "Leaving scale v3"))}]}]])
