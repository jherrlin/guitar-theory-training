(ns v2.se.jherrlin.music-theory.webapp.harmonizations
  (:require
   [v2.se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]
   ["semantic-ui-react" :as semantic-ui]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [clojure.string :as str]
   [clojure.set :as set]
   [v2.se.jherrlin.music-theory.definitions :as definitions]
   [se.jherrlin.music-theory :as music-theory]
   [v2.se.jherrlin.music-theory.intervals :as intervals]))


(def events-
  [{:n ::key-of}
   {:n ::major-or-minor}
   {:n ::triad-or-seventh}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn harmonization-view []
  (let [nr-of-frets      @(re-frame/subscribe [:nr-of-frets])
        key-of           @(re-frame/subscribe [::key-of])
        major-or-minor   @(re-frame/subscribe [::major-or-minor])
        triad-or-seventh @(re-frame/subscribe [::triad-or-seventh])]
    (when (and key-of major-or-minor triad-or-seventh)
      [:div


       [:div
        (for [{tone' :tone
               :keys [url-name title]}
              (->> (music-theory/find-root-p :a)
                   (map (fn [x] {:tone     x
                                 :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                 :title    (-> x name str/capitalize)})))]
          ^{:key url-name}
          [:div {:style {:margin-right "10px" :display "inline"}}
           [:a {:href (rfe/href :v2/harmonization {:key-of tone' :major-or-minor major-or-minor :triad-or-seventh triad-or-seventh})}
            [:button
             {:disabled (= key-of tone')}
             title]]])]
       [:br]
       [:div {:style {:display "flex"}}
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href :v2/harmonization {:key-of key-of :major-or-minor :major :triad-or-seventh triad-or-seventh})}
          [:button
           {:disabled (= major-or-minor :major)}
           "major"]]]
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href :v2/harmonization {:key-of key-of :major-or-minor :minor :triad-or-seventh triad-or-seventh})}
          [:button
           {:disabled (= major-or-minor :minor)}
           "minor"]]]]
       [:br]
       [:div {:style {:display "flex"}}
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href :v2/harmonization {:key-of key-of :major-or-minor major-or-minor :triad-or-seventh :triad})}
          [:button
           {:disabled (= triad-or-seventh :triad)}
           "triad"]]]
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href :v2/harmonization {:key-of key-of :major-or-minor major-or-minor :triad-or-seventh :seventh})}
          [:button
           {:disabled (= triad-or-seventh :seventh)}
           "seventh"]]]]

       [:h2 (str (-> key-of name str/capitalize) " - " (-> major-or-minor name) " - " (-> triad-or-seventh name))]

       [:br]
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (music-theory/diatonic-chord-progressions-str
          (music-theory/diatonic-chord-progressions-p key-of major-or-minor triad-or-seventh))]]

       [:br]

       ;; All tones in progressions
       [:h3 "All tones in harmonization"]
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (let [tones (->> (music-theory/diatonic-chord-progressions-p key-of major-or-minor triad-or-seventh)
                          (map :chord/tones)
                          (apply concat)
                          (set)
                          (vec))]
           (utils/fretboard-str
              (utils/fretboard-strings
               utils/rotate-until
               utils/all-tones
               [:e :b :g :d :a :e]
               nr-of-frets)
              (partial
               utils/fretboard-tone-str-chord-f tones)))
         ]]

       ;; Chords in progressions
       [:div
        (for [{chord-id        :chord/id
               chord-tones     :chord/tones
               chord-name      :chord/name
               chord-intervals :chord/intervals
               chord-tags      :chord/tags
               :as             m}
              (music-theory/diatonic-chord-progressions-p key-of major-or-minor triad-or-seventh)]
          ^{:key (str chord-name)}
          [:div
           [:a {:href (rfe/href :v2/chord {:key-of     (-> chord-tones first name)
                                           :chord-name (-> chord-id name)})}
            [:h3
             chord-name]]
           [:code
            [:pre {:style {:overflow-x "auto"}}
             (utils/fretboard-str
              (utils/fretboard-strings
               utils/rotate-until
               utils/all-tones
               [:e :b :g :d :a :e]
               nr-of-frets)
              (partial
               utils/fretboard-tone-str-chord-f chord-tones))]]])]])))

(def routes
  ["harmonization/:key-of/:major-or-minor/:triad-or-seventh"
   {:name :v2/harmonization
    :view [harmonization-view]
    :controllers
    [{:parameters {:path [:key-of :major-or-minor :triad-or-seventh]}
      :start      (fn [{{:keys [key-of major-or-minor triad-or-seventh]} :path}]
                    (let [key-of (-> key-of
                                     (str/lower-case)
                                     (str/replace "sharp" "#"))]
                      (js/console.log "Entering harmonization:" key-of major-or-minor triad-or-seventh)
                      (re-frame/dispatch [::key-of (keyword key-of)])
                      (re-frame/dispatch [::major-or-minor (keyword major-or-minor)])
                      (re-frame/dispatch [::triad-or-seventh (keyword triad-or-seventh)])))
      :stop       (fn [& params] (js/console.log "Leaving harmonization"))}]}])
