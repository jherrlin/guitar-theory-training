(ns v4.se.jherrlin.music-theory.webapp.notebook
  (:require
   [reitit.frontend.easy :as rfe]
   [v4.se.jherrlin.music-theory.webapp.piano.view :as piano.view]
   [v4.se.jherrlin.music-theory.webapp.strings.styled-fretboard :refer [styled-view]]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [v4.se.jherrlin.music-theory.webapp.params :as params]
   [re-frame.core :as re-frame]
   [clojure.string :as str]
   [malli.core :as m]))





(def faces
  {:css/piano      piano.view/piano-unit
   :css/fretboard  styled-view
   :text/fretboard (fn [data]
                     [:pre {:style {:overflow-x "auto"
                                    :margin     "0em"}}
                      (utils/fretboard-str
                       (fn [{:keys [out]}] (if (nil? out) "" out))
                       data)]
                     )})



;; [:button
;;  {:on-click
;;   #(re-frame/dispatch
;;     [:notebook/add
;;      {:id      (cljs.core/random-uuid)
;;       :url     ;; URL BACK TO CHORD / SCALE
;;       :title   ;;
;;       :version :v1
;;       :view    :css/piano
;;       :data    {:as-intervals   as-intervals
;;                 :index-tones    index-tones
;;                 :interval-tones interval-tones
;;                 :intervals      intervals
;;                 :key-of         key-of
;;                 :nr-of-octavs   nr-of-octavs
;;                 :title          (str (some-> interval-tones first name str/capitalize) sufix)
;;                 :text           "Chord"}}])}
;;  "add"]



(def events-
  [{:n :notebook}
   {:n :notebook/add
    :e (fn [db [k {:keys [id] :as v}]]
         (assoc-in db [:notebook id] v))}
   {:n :notebook/remove
    :e (fn [db [k v]]
         (update-in db [:notebook] dissoc v))}
   {:n :nodebook/ids
    :s (fn [db [k]]
         (->> (get db :notebook)
              (keys)
              (set)))}])

(comment
  @(re-frame/subscribe [:nodebook/ids])
  )

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub      n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))


(defn debug-view []
  (let [data @(re-frame/subscribe [:notebook])]
    [:pre
     (with-out-str (cljs.pprint/pprint data))]))

(defn notebook-view []
  (let [added  @(re-frame/subscribe [:notebook])
        debug? @(re-frame/subscribe [:debug])]
    [:div
     [:h3 "Notebook"]
     (when added
       (for [{:keys [view data id url title]} (vals added)]
         ^{:key (str id)}
         (let [{:keys [text]} data]
           [:div {:style {:margin-top "2em"
                          :display "flex"}}
            [(get faces view) data]
            [:div {:style {:display        "flex"
                           :flex-direction "column-reverse"}}

             [:button {:on-click #(re-frame/dispatch [:notebook/remove id])} "Remove"]
             [:a {:href (apply rfe/href url)}
              [:button  "Goto"]]
             [:div title]]])))
     (when debug?
       [debug-view])]))

(def routes
  ["/v4/notebook"
   {:name :v4/notebook
    :view [notebook-view]
    :controllers
    [{:parameters {:path  params/path-params
                   :query params/query-params}
      :start      (fn [{p :path q :query}]
                    (re-frame/dispatch [:path-params p])
                    (re-frame/dispatch [:query-params q]))}]}])
