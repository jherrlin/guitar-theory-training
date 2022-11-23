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
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]))



(def events-
  [{:n ::chord}
   {:n ::tone-or-interval
    :s (fn [db [n']] (get db n' :tone))}
   {:n ::as-intervals
    :s (fn [db [n']] (get db n' false))}
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
        query-params @(re-frame/subscribe [::query-params])]
    (when (and chord key-of)
      (let [{id          :chord/id
             indexes     :chord/indexes
             intervals   :chord/intervals
             explanation :chord/explanation
             sufix       :chord/sufix}
            (get @definitions/chords @(re-frame/subscribe [::chord]))]
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


         [piano.view/piano
         (if as-intervals
           :interval
           :tone)
         (utils/tones-by-key-and-intervals-2
          utils/all-tones
          key-of
          intervals)]
         ]
        ))))

(def routes
  [["/v4/piano/chord/:key-of/:chord-name"
    {:name :v4.piano/chords
     :view [chords-view]
     :controllers
     [{:parameters {:path  [:key-of :chord-name]
                    :query [:as-intervalls]}
       :start      (fn [{{:keys [key-of chord-name] :as p} :path
                         {:keys [as-intervalls] :as q}     :query}]
                     (js/console.log "path params" p)
                     (js/console.log "query params" q)
                     (re-frame/dispatch [::path-params p])
                     (re-frame/dispatch [::query-params q])
                     (let [key-of (-> key-of
                                      (str/lower-case)
                                      (str/replace "sharp" "#"))]
                       (re-frame/dispatch [:key-of (keyword key-of)])
                       (re-frame/dispatch [::chord (keyword chord-name)])
                       (re-frame/dispatch [::as-intervals (boolean (seq as-intervalls))])))}]}]])
