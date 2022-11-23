(ns v4.se.jherrlin.music-theory.webapp.piano.chords
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v4.se.jherrlin.music-theory.definitions :as definitions]
   [v4.se.jherrlin.music-theory.intervals :as intervals]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))



(def events-
  [{:n ::chord}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}
   {:n ::as-intervals
    :s (fn [db [n']] (get db n' false))}
   {:n ::nr-of-octavs}
   {:n ::path-params}
   {:n ::query-params}])

(comment
  @(re-frame/subscribe [::as-intervals])
  @re-frame.db/app-db
  )

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn chords-view []
  (let [chord        @(re-frame/subscribe [::chord])
        as-intervals @(re-frame/subscribe [::as-intervals])
        key-of       @(re-frame/subscribe [:key-of])
        path-params  @(re-frame/subscribe [::path-params])
        query-params @(re-frame/subscribe [::query-params])
        nr-of-octavs @(re-frame/subscribe [::nr-of-octavs])]
    (when (and chord key-of)
      (let [{id          :chord/id
             indexes     :chord/indexes
             intervals   :chord/intervals
             explanation :chord/explanation
             sufix       :chord/sufix}
            (get @definitions/chords @(re-frame/subscribe [::chord]))
            tones (utils/tones-by-key-and-intervals
                   utils/all-tones
                   key-of
                   intervals)]
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
             [:a {:href (rfe/href :v4.piano/chords (assoc path-params :key-of tone') query-params)}
              [:button
               {:disabled (= key-of tone')}
               title]]])]
         [:br]

         ;; Links to chords
         [:div
          (for [{id           :chord/id
                 sufix        :chord/sufix
                 display-text :chord/display-text}
                (->> @definitions/chords
                     vals
                     (sort-by :chord/order))]
            ^{:key (str id sufix "-chord")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href :v4.piano/chords (assoc path-params :chord-name id) query-params)}
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
                       (rotate-until #(% key-of))
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

         ;; Intervals
         [:pre {:style {:overflow-x "auto"}}
          (->> (map
                (fn [interval index]
                  (str (fformat "%8s" interval) " -> " (-> index name str/capitalize)))
                intervals
                tones)
               (str/join "\n")
               (apply str)
               (str "Interval -> Tone\n"))]


         [:br]

         [:div {:style {:display :flex}}
          (for [index (range 0 nr-of-octavs)]
            ^{:key (str "piano-octav-index-" index)}
            [piano.view/piano
             (if as-intervals
               :interval
               :tone)
             (utils/tones-by-key-and-intervals-2
              utils/all-tones
              key-of
              intervals)]
            )]]))))

(def routes
  [["/v4/piano/chord/:key-of/:chord-name"
    {:name :v4.piano/chords
     :view [chords-view]
     :controllers
     [{:parameters {:path  [:key-of :chord-name]
                    :query [:as-intervalls :octavs]}
       :start      (fn [{{:keys [key-of chord-name] :as p}    :path
                         {:keys [as-intervalls octavs] :as q} :query}]
                     (js/console.log "path params" p)
                     (js/console.log "query params" q)
                     (re-frame/dispatch [::path-params p])
                     (re-frame/dispatch [::query-params q])
                     (let [octavs (if-not octavs
                                    2
                                    (js/parseInt octavs))
                           key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (re-frame/dispatch [::nr-of-octavs octavs])
                       (re-frame/dispatch [:key-of (keyword key-of)])
                       (re-frame/dispatch [::chord (keyword chord-name)])
                       (re-frame/dispatch [::as-intervals (boolean (seq as-intervalls))])))}]}]])
