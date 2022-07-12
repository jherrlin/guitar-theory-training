(ns v2.se.jherrlin.music-theory.webapp.chords
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
   [se.jherrlin.music-theory :as music-theory]))


(def events-
  [{:n ::key-of}
   {:n ::chord}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(apply concat [#{:a :b} #{:c} #{:c :d}])

(defn chords-view []
  (let [chord  @(re-frame/subscribe [::chord])
        key-of @(re-frame/subscribe [::key-of])
        {chord-id    :chord/id
         indexes     :chord/indexes
         intervals   :chord/intervals
         explanation :chord/explanation
         sufix       :chord/sufix}
        (get-in @definitions/chords-atom [chord])]
    (when (and chord key-of)
      (let [tones ((utils/juxt-indexes-and-intervals indexes intervals)
                    (utils/rotate-until #(% key-of) utils/all-tones))]
        [:div
         [:div "chord:"]
         [:p sufix]
         [:p explanation]

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

         ;; All tones in chord
         [:h3 "All tone positions in the chord"]
         [:pre {:style {:overflow-x "auto"}}
          (utils/fretboard-str
             (utils/fretboard-strings
              utils/rotate-until
              utils/all-tones
              [:e :b :g :d :a :e]
              25)
             (partial
              utils/fretboard-tone-str-chord-f tones))]]))))

(def routes
  ["chord/:key-of/:chord-name"
   {:name      ::chord-view
    :view      [chords-view]
    :link-text "chord-view"
    :controllers
    [{:parameters {:path [:key-of :chord-name]}
      :start      (fn [{{:keys [key-of chord-name]} :path}]
                    (let [key-of (-> key-of
                                     (str/lower-case)
                                     (str/replace "sharp" "#"))]
                      (js/console.log "Entering chord:" key-of chord-name)
                      (re-frame/dispatch [::key-of (keyword key-of)])
                      (re-frame/dispatch [::chord (keyword chord-name)])))
      :stop       (fn [& params] (js/console.log "Leaving..."))}]}])
