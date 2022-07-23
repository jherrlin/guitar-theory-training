(ns v2.se.jherrlin.music-theory.webapp.scales
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [reitit.frontend.easy :as rfe]
   [se.jherrlin.music-theory :as music-theory]
   [v2.se.jherrlin.music-theory.definitions :as definitions]
   [v2.se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(def events-
  [{:n ::key-of}
   {:n ::scale}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn scales-view []
  (let [nr-of-frets @(re-frame/subscribe [:nr-of-frets])
        scale       @(re-frame/subscribe [::scale])
        key-of      @(re-frame/subscribe [::key-of])]
    (when (and scale key-of)
      (let [{intervals :scale/intervals
             indexes   :scale/indexes
             sufix     :scale/sufix}
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
             [:a {:href (rfe/href :v2/scale {:scale scale :key-of key-of'})}
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
             [:a {:href (rfe/href :v2/scale {:scale id :key-of key-of})}
              [:button
               {:disabled (= scale id)}
               title]]])]
         [:br]

         ;; Scale name
         (let [{title :scale/title}
               (get @definitions/scales-atom :diminished-whole #_scale)]
           [:div
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
         [:code
          [:pre {:style {:overflow-x "auto"}}
           (utils/fretboard-str
            (utils/fretboard-strings
             utils/rotate-until
             utils/all-tones
             [:e :b :g :d :a :e]
             nr-of-frets)
            (partial
             utils/fretboard-tone-str-chord-f tones))]]

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
            [:a {:href (rfe/href :v2/chord {:chord-name chord-id :key-of key-of})}
             [:button chord-title]]])]))))

(def routes
  ["scale/:scale/:key-of"
   {:name :v2/scale
    :view [scales-view]
    :controllers
    [{:parameters {:path [:scale :key-of]}
      :start      (fn [{{:keys [scale key-of]} :path}]
                    (let [scale'  (keyword scale)
                          key-of' (keyword key-of)]
                      (js/console.log "Entering scale:" scale key-of)
                      (re-frame/dispatch [::scale scale'])
                      (re-frame/dispatch [::key-of key-of'])))
      :stop       (fn [& params] (js/console.log "Leaving scale"))}]}])
