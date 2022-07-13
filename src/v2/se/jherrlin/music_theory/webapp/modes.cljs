(ns v2.se.jherrlin.music-theory.webapp.modes
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
   {:n ::scale}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn mode-view []
  (let [scale  @(re-frame/subscribe [::scale])
        key-of @(re-frame/subscribe [::key-of])]
    (when (and scale key-of)
      [:div
       [:p "modes"]
       [:div (str scale)]
       [:div (str key-of)]])
    )
  )

(def routes
  ["mode/:scale/:key-of"
   {:name ::mode
    :view [mode-view]
    :controllers
    [{:parameters {:path [:scale :key-of]}
      :start      (fn [{{:keys [scale key-of]} :path}]
                    (let [scale'  (keyword scale)
                          key-of' (keyword key-of)]
                      (js/console.log "Entering mode:" scale key-of)
                      (re-frame/dispatch [::scale scale'])
                      (re-frame/dispatch [::key-of key-of'])))
      :stop       (fn [& params] (js/console.log "Leaving mode"))}]}])
