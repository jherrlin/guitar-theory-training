(ns se.jherrlin.music-theory.webapp.main
  (:require
   ["semantic-ui-react" :as semantic-ui]
   [se.jherrlin.music-theory :as music-theory]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]
   [reitit.core :as r]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [clojure.string :as str]))


;;; Effects ;;;

;; Triggering navigation from events.

(re-frame/reg-fx :push-state
  (fn [route]
    (apply rfe/push-state route)))

;;; Events ;;;

(re-frame/reg-event-db ::initialize-db
  (fn [db _]
    (if db
      db
      {:current-route nil})))

(re-frame/reg-event-fx ::push-state
  (fn [_ [_ & route]]
    {:push-state route}))

(re-frame/reg-event-db ::navigated
  (fn [db [_ new-match]]
    (let [old-match   (:current-route db)
          controllers (rfc/apply-controllers (:controllers old-match) new-match)]
      (assoc db :current-route (assoc new-match :controllers controllers)))))

;;; Subscriptions ;;;

(re-frame/reg-sub :current-route
  (fn [db]
    (:current-route db)))

;;; Views ;;;

(defn home-page []
  [:div
   [:h1 "This is home page"]
   [:button
    ;; Dispatch navigate event that triggers a (side)effect.
    {:on-click #(re-frame/dispatch [::push-state ::harmonization])}
    "Go to sub-page 2"]])

(def events-
  [{:n ::tone}
   {:n ::chord}
   {:n :harmonization/tone}
   {:n :harmonization/major-or-minor}
   {:n :harmonization/triad-or-seventh}
   {:n ::scale}
   {:n ::key}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db _] (get db n))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))


(defn ^:dev/after-load header-menu [router]
  (let [current-route      @(re-frame/subscribe [:current-route])
        current-route-url  (get current-route :template "")]
    [:div {:style {:flex "1"}}
     [:> semantic-ui/Menu {:size       "small"
                           :borderless true
                           :style      {:background "#FFFFFF"}}
      [:> semantic-ui/Menu.Item
       {:as   "a"
        :href (rfe/href ::chord-tones-redirect)}
       "Chords"]
      [:> semantic-ui/Menu.Item
         {:as   "a"
          :href (rfe/href ::scale-redirect)}
       "Scales"]
      [:> semantic-ui/Menu.Item
       {:as   "a"
        :href (rfe/href ::harmonization {:tone :c :major-or-minor :major :triad-or-seventh :triad})
        }
       "Harmonizations"]]]))

(some->> @(re-frame/subscribe [:current-route])
         :data :view)

(defn ^:dev/after-load main [router]
  (let [current-route @(re-frame/subscribe [:current-route])
        sub-menu      (some-> current-route :data :sub-menu)]
    [:<>
     (when current-route
       [:div
        {:style {:height         "100%"
                 :display        "flex"
                 :flex-direction "column"}}

        [header-menu router]

        #_(when sub-menu
          [:div
           [:> semantic-ui/Menu {:secondary true}
            [sub-menu router]]])

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
            (-> current-route :data :view))]]

        #_[:div
         [:> semantic-ui/Menu {:size "small"}
          [:> semantic-ui/Popup
           {:position "top right"
            :content  "Notifications history."}]]]])]))



(->> @music-theory/chords-atom vals (map :chord/sufix))

(->> @music-theory/scales-atom vals)

(map :scale/title)


(defn harmonization-view []
  (let [tone             @(re-frame/subscribe [:harmonization/tone])
        major-or-minor   @(re-frame/subscribe [:harmonization/major-or-minor])
        triad-or-seventh @(re-frame/subscribe [:harmonization/triad-or-seventh])]
    (when (and tone major-or-minor triad-or-seventh)
      [:div

       [:div {:style {:display "flex"}}
          (for [{:keys [tone url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x name str/upper-case)})))]
            ^{:key url-name}
            [:div {:style {:margin-left "10px"}}
             [:a {:href (rfe/href ::harmonization {:tone tone :major-or-minor major-or-minor :triad-or-seventh triad-or-seventh})} title]])]



       [:code
        [:pre
         (music-theory/diatonic-chord-progressions-str
          (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh))]]
       [:div
        [:p "All tones"]
        [:code
         [:pre
          (music-theory/fret-table-with-tones-p
           (->> (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh)
                (map :chord/tones)
                (apply concat)
                (set)
                (vec)))]]]
       [:div
        (for [{chord-tones :chord/tones
               chord-name  :chord/name
               chord-intervals :chord/intervals}
              (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh)]
          ^{:key (str chord-name)}
          [:div
           [:p chord-name " | " chord-intervals]
           [:code
            [:pre
             (music-theory/fret-table-with-tones-p chord-tones)]]])]

       [:div
        [:code
         [:pre
          (with-out-str (cljs.pprint/pprint (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh)))]]]])))

;;; Routes ;;;

(defn href
  "Return relative url for given route. Url can be used in HTML links."
  ([k]
   (href k nil nil))
  ([k params]
   (href k params nil))
  ([k params query]
   (rfe/href k params query)))

(defn chord-tones-view []
  (let [chord @(re-frame/subscribe [::chord])
        tone  @(re-frame/subscribe [::tone])]
    (when (and chord tone)
      (let [{intervals :chord/intervals
             sufix     :chord/sufix}
            (get-in @music-theory/chords-atom [@(re-frame/subscribe [::chord])])]
        [:div
         [:div {:style {:display "flex"}}
          (for [{:keys [tone url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x name str/upper-case)})))]
            ^{:key url-name}
            [:div {:style {:margin-left "10px"}}
             [:a {:href (rfe/href ::chord-tones {:tone tone :chord-name chord})} title]])]

         [:div {:style {:display "flex"}}
          (for [{id    :chord/id
                 sufix :chord/sufix
                 explanation :chord/explanation}
                (->> @music-theory/chords-atom vals)]
            ^{:key (str sufix "-chord")}
            [:div {:style {:margin-left "10px"}}
             [:a {:href (rfe/href ::chord-tones {:tone tone :chord-name id})} (str sufix " (" explanation ")")]])]

         [:p intervals]
         [:p (str (-> tone name str/upper-case) sufix)]
         [:code
          [:pre
           (music-theory/fret-table-with-tones-p
            ((get-in @music-theory/chords-atom [@(re-frame/subscribe [::chord]) :chord/f])
             (music-theory/find-root-p @(re-frame/subscribe [::tone]))))]]]))))



(defn scale-in-key-view []
  (let [scale @(re-frame/subscribe [::scale])
        key   @(re-frame/subscribe [::key])]
    (when (and scale key)
      [:div

       [:div {:style {:display "flex"}}
        (for [{:keys [tone url-name title]}
              (->> (music-theory/find-root-p :a)
                   (map (fn [x] {:tone     x
                                 :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                 :title    (-> x name str/upper-case)})))]
          ^{:key url-name}
          [:div {:style {:margin-left "10px"}}
           [:a {:href (rfe/href ::scale {:scale scale :key tone})} title]])]


       [:div {:style {:display "flex"}}
        (for [{intervals :scale/intervals
               title     :scale/title
               id        :scale/id}
              (->> @music-theory/scales-atom vals)]
          ^{:key (str title "-scale")}
          [:div {:style {:margin-left "10px"}}
           [:a {:href (rfe/href ::scale {:scale id :key key})} title]])]

       (let [{intervals :scale/intervals
              title :scale/title }
             (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
         [:div
          [:h3 title]
          [:h5 intervals]])

       [:code
        [:pre
         (music-theory/fret-table-with-tones-p
          ((get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale]) :scale/f])
           (music-theory/find-root-p @(re-frame/subscribe [::key]))))]]])))



(def routes
  ["/"
   [""
    {:name      ::home
     :view      [home-page]
     :link-text "Home"
     :controllers
     [{;; Do whatever initialization needed for home page
       ;; I.e (re-frame/dispatch [::events/load-something-with-ajax])
       :start (fn [& params](js/console.log "Entering home page"))
       ;; Teardown can be done here.
       :stop  (fn [& params] (js/console.log "Leaving home page"))}]}]
   ["chord"
    {:name      ::chord-tones-redirect
     :controllers
     [{:start (fn [_]
                (js/console.log "Chord redirect")
                (re-frame/dispatch [::push-state ::chord-tones {:tone "c" :chord-name "major"}]))
       :stop  (fn [& params] (js/console.log "Leaving..."))}]}]
   ["chord/:tone/:chord-name"
    {:name      ::chord-tones
     :view      [chord-tones-view]
     :link-text "chord-tones"
     :controllers
     [{:parameters {:path [:tone :chord-name]}
       :start      (fn [{{:keys [tone chord-name]} :path}]
                     (let [tone (-> tone
                                    (str/lower-case)
                                    (str/replace "sharp" "#"))]
                       (js/console.log "Entering chord:" tone chord-name)
                       (re-frame/dispatch [::tone (keyword tone)])
                       (re-frame/dispatch [::chord (keyword chord-name)])))
       :stop       (fn [& params] (js/console.log "Leaving..."))}]}]

   ["scale"
    {:name      ::scale-redirect
     :controllers
     [{:start (fn [_]
                (js/console.log "Scale redirect")
                (re-frame/dispatch [::push-state ::scale {:scale :major :key :c}]))
       :stop  (fn [& params] (js/console.log "Leaving scale redirect..."))}]}]
   ["scale/:scale/:key"
    {:name      ::scale
     :view      [scale-in-key-view]
     :link-text "chord-tones"
     :controllers
     [{:parameters {:path [:scale :key]}
       :start      (fn [{{:keys [scale key]} :path}]
                     (let [scale' (keyword scale)
                           key'   (keyword key)]
                       (js/console.log "Entering scale:" scale key)
                       (re-frame/dispatch [::scale scale'])
                       (re-frame/dispatch [::key key'])))
       :stop       (fn [& params] (js/console.log "Leaving scale"))}]}]

   ["harmonization/:tone/:major-or-minor/:triad-or-seventh"
    {:name      ::harmonization
     :view      [harmonization-view]
     :link-text "Harmonization"
     :controllers
     [{:parameters {:path [:tone :major-or-minor :triad-or-seventh]}
       :start      (fn [{{:keys [tone major-or-minor triad-or-seventh]} :path}]
                     (let [tone (-> tone
                                    (str/lower-case)
                                    (str/replace "sharp" "#"))]
                       (js/console.log "Entering harmonization:" tone major-or-minor triad-or-seventh)
                       (re-frame/dispatch [:harmonization/tone (keyword tone)])
                       (re-frame/dispatch [:harmonization/major-or-minor (keyword major-or-minor)])
                       (re-frame/dispatch [:harmonization/triad-or-seventh (keyword triad-or-seventh)])))
       :stop       (fn [& params] (js/console.log "Leaving harmonization"))}]}]])

(defn on-navigate [new-match]
  (when new-match
    (re-frame/dispatch [::navigated new-match])))

(def router
  (rf/router
    routes
    {:data {:coercion rss/coercion}}))

(defn init-routes! []
  (js/console.log "initializing routes")
  (rfe/start!
    router
    on-navigate
    {:use-fragment true}))

(defn nav [{:keys [router current-route]}]
  [:ul
   (for [route-name (r/route-names router)
         :let       [route (r/match-by-name router route-name)
                     text (-> route :data :link-text)]]
     [:li {:key route-name}
      (when (= route-name (-> current-route :data :name))
        "> ")
      ;; Create a normal links that user can click
      [:a {:href (href route-name)} text]])])

(defn router-component [{:keys [router]}]
  (let [current-route @(re-frame/subscribe [:current-route])]
    [:div
     [nav {:router router :current-route current-route}]
     (when current-route
       [(-> current-route :data :view)])]))

;;; Setup ;;;

(def debug? ^boolean goog.DEBUG)

(defn dev-setup []
  (when debug?
    (enable-console-print!)
    (println "dev mode")))


(defn main-component []
  [:div
   [:code
    [:pre (music-theory/fret-table-with-tones-p
           [:a :b :c])]]
   ])

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (rd/render
   ;; [main-component]
   ;; [router-component {:router router}]
   [main router]
   (.getElementById js/document "app")))



(defn ^:dev/after-load init []
  (println "starting...")
  (re-frame/clear-subscription-cache!)
  (re-frame/dispatch-sync [::initialize-db])
  (dev-setup)
  (init-routes!) ;; Reset routes on figwheel reload
  (mount-root)
  )
