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
  (let [key-of           @(re-frame/subscribe [::key-of])
        major-or-minor   @(re-frame/subscribe [::major-or-minor])
        triad-or-seventh @(re-frame/subscribe [::triad-or-seventh])]
    (when (and key-of major-or-minor triad-or-seventh)
      [:div
       [:p "harmonization view"]
       [:div key-of]
       [:div major-or-minor]
       [:div triad-or-seventh]])))

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
