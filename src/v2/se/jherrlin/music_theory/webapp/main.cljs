(ns v2.se.jherrlin.music-theory.webapp.main
  (:require
   [v2.se.jherrlin.music-theory.webapp.events :as events]
   [v2.se.jherrlin.music-theory.webapp.chords :as chords]
   ["semantic-ui-react" :as semantic-ui]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [clojure.string :as str]
   [clojure.set :as set]))


(defn on-navigate [new-match]
  (when new-match
    (re-frame/dispatch [::events/navigated new-match])))

(defn index []
  [:div "hejsan"])

(def routes
  ["/v2/"
   [""
    {:name      ::v2-home
     :view      [index]}]
   chords/routes])

(def router
  (rf/router
   routes
   {:data {:coercion rss/coercion}}))

(defn ^:dev/after-load header-menu []
  (let [current-route      @(re-frame/subscribe [:current-route])
        current-route-name (get-in current-route [:data :name])]
    (when (and current-route current-route-name)
      [:div {:style {:flex "1"}}
       [:> semantic-ui/Menu {:size       "small"
                             :borderless true
                             :style      {:background "#FFFFFF"}}
        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::the-neck)
          :active (= ::the-neck current-route-name)}
         "Neck"]

        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::chord-tones-redirect)
          :active (= ::chord-tones current-route-name)}
         "Chords"]
        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::scale-redirect)
          :active (= ::scale current-route-name)}
         "Scales"]
        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::harmonization {:tone :c :major-or-minor :major :triad-or-seventh :triad})
          :active (= ::harmonization current-route-name)}
         "Harmonizations"]

        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::mode {:scale :ionian :key :c})
          :active (= ::mode current-route-name)}
         "Modes"]

        [:> semantic-ui/Menu.Item
         {:as     "a"
          :href   (rfe/href ::drills)
          :active (= ::drills current-route-name)}
         "Drills"]

        [:> semantic-ui/Menu.Menu {:position "right"}
         [:> semantic-ui/Menu.Item
          {:as     "a"
           :href   "https://github.com/jherrlin/guitar-theory-training"
           :target "_blank"}
          "Source code"]]]])))

(defn ^:dev/after-load main []
  (let [current-route @(re-frame/subscribe [:current-route])]
    [:<>
     (when current-route
       [:div
        [header-menu]

        [:div {:style {:height     "100%"
                       :overflow-y "auto"
                       :overflow-x "hidden"}}
         [:> semantic-ui/Segment
          {:basic true
           :style {:height "100%"}}
          [:> semantic-ui/Dimmer
           {:inverted true
            :active   false}
           [:> semantic-ui/Loader
            {:inline  true
             :content false}]]

          ;; This is the main location on the page.
          (when current-route
            (-> current-route :data :view))]]])]))

(defn init-routes! []
  (js/console.log "initializing routes")
  (rfe/start!
   router
   on-navigate
   {:use-fragment true}))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (rd/render
   [main]
   (.getElementById js/document "app")))

(def debug? ^boolean goog.DEBUG)

(defn dev-setup []
  (when debug?
    (enable-console-print!)
    (println "dev mode")))

(defn ^:dev/after-load init []
  (println "starting...")
  (re-frame/clear-subscription-cache!)
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (init-routes!) ;; Reset routes on figwheel reload
  (mount-root))
