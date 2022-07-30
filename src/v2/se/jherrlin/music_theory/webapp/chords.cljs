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
  [{:n ::key-of}
   {:n ::chord}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn chords-view []
  (let [nr-of-frets      @(re-frame/subscribe [:nr-of-frets])
        tuning-name      @(re-frame/subscribe [:tuning-name])
        chord            @(re-frame/subscribe [::chord])
        key-of           @(re-frame/subscribe [::key-of])
        tone-or-interval @(re-frame/subscribe [::tone-or-interval])
        {indexes     :chord/indexes
         intervals   :chord/intervals
         explanation :chord/explanation
         sufix       :chord/sufix}
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
             [:a {:href (rfe/href :v2/chord {:key-of tone' :chord-name chord})}
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
            ^{:key (str sufix "-chord")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v2/chord {:key-of key-of :chord-name id})}
              [:button
               {:disabled (= id chord)}
               (str (or display-text sufix))]]])]

         ;; Chord name
         [:br]
         [:div {:style {:height  "100%"
                        :display "inline-flex"}}
          [:h2 (str (-> key-of name str/capitalize) sufix)]
          [:p {:style {:margin-left "4em"
                       :margin-top  "0.5em"}}
           (str "(" explanation ")")]]

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
            (if (= :guitar tuning-name)
              definitions/standard-guitar-tuning
              definitions/standard-ukulele-tuning)
            nr-of-frets)
           (if (= tone-or-interval :tone)
             (partial
              utils/fretboard-tone-str-chord-f tones)
             (partial
              utils/fretboard-tone-str-chord-f-2
              (mapv vector tones intervals))))]

         ;; Chord patterns
         (let [tuning-tones (if (:guitar tuning-name)
                              definitions/standard-guitar-tuning
                              definitions/standard-ukulele-tuning)
               chord-patterns (->> @definitions/chord-patterns-atom
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
                      (if (= :guitar tuning-name)
                        definitions/standard-guitar-tuning
                        definitions/standard-ukulele-tuning)
                      nr-of-frets)
                     key-of
                     pattern)
                    (if (= tone-or-interval :tone)
                      utils/fretboard-tone-str-pattern-f
                      utils/fretboard-interval-f))]])]]))

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
                 [:a {:href (rfe/href :v2/scale {:scale scale-id :key-of key-of})}
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
                                            :instrumnt  @(re-frame/subscribe [:tuning-name])}])))
       :stop       (fn [& params] (js/console.log "Leaving chords..."))}]}]
   ["/v3/:instrumnt/chord/:key-of/:chord-name"
    {:name :v3/chord
     :view [chords-view]
     :controllers
     [{:parameters {:path [:key-of :chord-name :instrumnt]}
       :start      (fn [{{:keys [key-of chord-name instrumnt]} :path}]
                     (let [key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (js/console.log "Entering chord:" key-of chord-name)
                       (re-frame/dispatch [::key-of (keyword key-of)])
                       (re-frame/dispatch [::chord (keyword chord-name)])
                       (re-frame/dispatch [:tuning-name (keyword instrumnt)])))
       :stop       (fn [& params] (js/console.log "Leaving chords..."))}]}]])
