(ns se.jherrlin.music-theory.webapp.main
  (:require
   [se.jherrlin.drills :as drills]
   [se.jherrlin.music-theory.utils
    :refer [fformat tone->str]
    :as utils]
   ["semantic-ui-react" :as semantic-ui]
   [se.jherrlin.music-theory :as music-theory]
   [reagent.dom :as rd]
   [re-frame.core :as re-frame]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rf]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [clojure.string :as str]
   [clojure.set :as set]
   [v2.se.jherrlin.music-theory.webapp.harmonizations :as harmonizations]
   [v2.se.jherrlin.music-theory.webapp.chords :as chords]
   [v2.se.jherrlin.music-theory.webapp.main :as v2-main]))


(re-frame/reg-fx
 :push-state
 (fn [route]
   (apply rfe/push-state route)))

(re-frame/reg-event-db
 ::initialize-db
 (fn [db _]
   (if db
     db
     {:current-route nil})))

(re-frame/reg-event-fx
 ::push-state
 (fn [_ [_ & route]]
   {:push-state route}))

(re-frame/reg-event-db
 ::navigated
 (fn [db [_ new-match]]
   (let [old-match   (:current-route db)
         controllers (rfc/apply-controllers (:controllers old-match) new-match)]
     (assoc db :current-route (assoc new-match :controllers controllers)))))

(re-frame/reg-sub
 :current-route
 (fn [db]
   (:current-route db)))

(defn home-page []
  [:div
   [:h1 "Guitar theory"]
   [:div
    "In the summer of 2022 I decided to learn a bit of music / guitar theory. "
    "As a " [:a {:href "https://www.gnu.org/software/emacs/" :target "_blank"} "Emacs"]
    " user I decided to use my favorite tool to help me out. Ive been using (1) "
    [:a {:href "https://orgmode.org/worg/org-contrib/org-drill.html" :target "_blank"} "org-drill"]
    " a couple of times when trying to learn new stuff and I really like it. This
project started out as a tool that could generate org-drill questions and
anwsers into a plain text file with "
    [:a {:href "https://orgmode.org/" :target "_blank"} "org mode"]
    " structure. The org-drill setup was very useful but I needed to quickly get an
overview of scales, chords and modes. So I made a small frontend. The source
code lives on "
    [:a {:href "https://github.com/jherrlin/guitar-theory-training" :target "_blank"} "GitHub"]
    " and you are more than welcome to improve the project. Please send a pull
request."]
   [:br]
   [:br]
   [:div "! Everything is using sharps (#) and never flats (b). This may break some
general rules but it's how it's implemented here. Feel free to improve!"]
   [:br]
   [:div "In the webapp you can browse guitar chords, scales, modes and more. And yoy can
generate org-drill text files that you can use in Emacs to help you learn."]
   [:br] [:br]
   [:div "1. org-drill is a Emacs mode where you specify questions and answers in a
specific text format and a spaced repetition algorithm selects questions."]])

(def events-
  [{:n ::tone}
   {:n ::chord}
   {:n :harmonization/tone}
   {:n :harmonization/major-or-minor}
   {:n :harmonization/triad-or-seventh}
   {:n ::scale}
   {:n ::key}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(->> @(re-frame/subscribe [:current-route])
     :data :name)

(defn ^:dev/after-load header-menu [router]
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
          :href   (rfe/href :v2/chord {:key-of :c :chord-name :major})
          :active (= :v2/chord current-route-name)}
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
          :href   (rfe/href :v2/mode {:scale :ionian :key-of :c})
          :active (= :v2/mode current-route-name)}
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

(defn ^:dev/after-load main [router]
  (let [current-route @(re-frame/subscribe [:current-route])]
    [:<>
     (when current-route
       [:div
        [header-menu router]

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

(defn harmonization-view []
  (let [tone             @(re-frame/subscribe [:harmonization/tone])
        major-or-minor   @(re-frame/subscribe [:harmonization/major-or-minor])
        triad-or-seventh @(re-frame/subscribe [:harmonization/triad-or-seventh])]
    (when (and tone major-or-minor triad-or-seventh)
      [:div

       [:div
        (for [{tone' :tone
               :keys [url-name title]}
              (->> (music-theory/find-root-p :a)
                   (map (fn [x] {:tone     x
                                 :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                 :title    (-> x name tone->str)})))]
          ^{:key url-name}
          [:div {:style {:margin-right "10px" :display "inline"}}
           [:a {:href (rfe/href ::harmonization {:tone tone' :major-or-minor major-or-minor :triad-or-seventh triad-or-seventh})}
            [:button
             {:disabled (= tone tone')}
             title]]])]
       [:br]
       [:div {:style {:display "flex"}}
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href ::harmonization {:tone tone :major-or-minor :major :triad-or-seventh triad-or-seventh})}
          [:button
           {:disabled (= major-or-minor :major)}
           "major"]]]
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href ::harmonization {:tone tone :major-or-minor :minor :triad-or-seventh triad-or-seventh})}
          [:button
           {:disabled (= major-or-minor :minor)}
           "minor"]]]]
       [:br]
       [:div {:style {:display "flex"}}
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href ::harmonization {:tone tone :major-or-minor major-or-minor :triad-or-seventh :triad})}
          [:button
           {:disabled (= triad-or-seventh :triad)}
           "triad"]]]
        [:div {:style {:margin-right "10px"}}
         [:a {:href (rfe/href ::harmonization {:tone tone :major-or-minor major-or-minor :triad-or-seventh :seventh})}
          [:button
           {:disabled (= triad-or-seventh :seventh)}
           "seventh"]]]]

       [:h2 (str (-> tone tone->str) " - " (-> major-or-minor name) " - " (-> triad-or-seventh name))]

       [:br]
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (music-theory/diatonic-chord-progressions-str
          (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh))]]
       [:div
        [:h3 "All tones"]
        [:code
         [:pre {:style {:overflow-x "auto"}}
          (music-theory/fret-table-with-tones-p
           (->> (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh)
                (map :chord/tones)
                (apply concat)
                (set)
                (vec))
           16)]]]
       [:div
        (for [{chord-id        :chord/id
               chord-tones     :chord/tones
               chord-name      :chord/name
               chord-intervals :chord/intervals
               chord-tags      :chord/tags
               :as             m}
              (music-theory/diatonic-chord-progressions-p tone major-or-minor triad-or-seventh)]
          ^{:key (str chord-name)}
          [:div
           [:h3
            {:on-click
             #(re-frame/dispatch
               [::push-state
                ::chord-tones
                {:tone (-> chord-tones first name)
                 :chord-name (-> chord-id name)}])}
            chord-name]
           [:code
            [:pre {:style {:overflow-x "auto"}}
             (music-theory/fret-table-with-tones-p chord-tones 16)]]])]])))

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
      (let [{chord-id      :chord/id
             chord-indexes :chord/indexes
             intervals     :chord/intervals
             explanation   :chord/explanation
             intervals-xs  :chord/intervals-xs
             sufix         :chord/sufix}
            (get-in @music-theory/chords-atom [@(re-frame/subscribe [::chord])])]
        [:div
         [:div
          (for [{tone' :tone
                 :keys [url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x tone->str)})))]
            ^{:key url-name}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::chord-tones {:tone tone' :chord-name chord})}
              [:button
               {:disabled (= tone tone')}
               title]]])]
         [:br]
         [:div
          (for [{id           :chord/id
                 sufix        :chord/sufix
                 explanation  :chord/explanation
                 display-text :chord/display-text}
                (->> @music-theory/chords-atom vals)]
            ^{:key (str sufix "-chord")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::chord-tones {:tone tone :chord-name id})}
              [:button
               {:disabled (= id chord)}
               (str (or display-text sufix))]]])]
         [:br]
         [:div {:style {:height  "100%"
                        :display "inline-flex"}}
          [:h2 (str (-> tone tone->str) sufix)]
          [:p {:style {:margin-left "4em"
                       :margin-top  "0.5em"}}
           (str "(" explanation ")")]]

         [:pre {:style {:overflow-x "auto"}}
          (->> (map
                (fn [i x]
                  (str (fformat "%8s" i) " -> " (-> (music-theory/interval->tone tone i) tone->str)))
                intervals-xs
                ((get-in @music-theory/chords-atom [@(re-frame/subscribe [::chord]) :chord/f])
                 (music-theory/find-root-p @(re-frame/subscribe [::tone]))))
               (str/join "\n")
               (apply str)
               (str "Interval -> Tone\n"))]

         [:h3 "All tone positions in the chord"]
         [:pre {:style {:overflow-x "auto"}}
          (->> (music-theory/intervals-and-key-to-fretboard-matrix
                music-theory/standard-tuning
                tone
                intervals-xs
                16)
               (music-theory/intervals-and-key-to-fretboard-matrix-str))]

         [:h3 "Chord patterns"]
         [:div
          (for [{id :chord/pattern-id}
                (->> @music-theory/chord-patterns-atom
                     vals
                     (filter (comp #{@(re-frame/subscribe [::chord])} :chord-pattern/name)))]
            ^{:key (-> id name)}
            [:div {:style {:margin-top "2em"}}
             [:pre {:style {:overflow-x "auto"}}
              (music-theory/chord-pattern-str @music-theory/chord-patterns-atom id tone)]])]

         [:h3 "Scales to chord"]
         (for [{scale-title :scale/title
                scale-id    :scale/id}
               (let [{chord-indexes :chord/indexes}
                     (get-in @music-theory/chords-atom [@(re-frame/subscribe [::chord])])]
                 (->> (vals @music-theory/scales-atom)
                      (filter (fn [{:scale/keys [indexes]}]
                                (set/subset? (set chord-indexes) (set indexes))))))]
           ^{:key scale-title}
           [:div {:style {:margin-right "10px" :display "inline"}}
            [:a {:href (rfe/href ::scale {:scale scale-id :key tone})}
             [:button scale-title]]])]))))

(defn scale-in-key-view []
  (let [scale @(re-frame/subscribe [::scale])
        key   @(re-frame/subscribe [::key])]
    (when (and scale key)
      (let [{intervals    :scale/intervals
             indexes      :scale/indexes
             intervals-xs :scale/intervals-xs
             sufix        :scale/sufix}
            (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
        [:div

         [:div
          (for [{:keys [url-name tone title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x tone->str)})))]
            ^{:key url-name}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::scale {:scale scale :key tone})}
              [:button
               {:disabled (= tone key)}
               title]]])]

         [:br]
         [:div
          (for [{intervals :scale/intervals
                 title     :scale/title
                 id        :scale/id}
                (->> @music-theory/scales-atom vals)]
            ^{:key (str title "-scale")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::scale {:scale id :key key})}
              [:button
               {:disabled (= scale id)}
               title]]])]

         [:br]

         (let [{intervals :scale/intervals
                title     :scale/title }
               (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
           [:div
            [:h2 (str (-> key tone->str) " - " (-> title str/capitalize))]])

         [:pre {:style {:overflow-x "auto"}}
          (->> (map
                (fn [i]
                  (str (fformat "%8s" i) " -> " (-> (music-theory/interval->tone key i) tone->str)))
                intervals-xs)
               (str/join "\n")
               (apply str)
               (str "Interval -> Tone\n"))]

         [:code
          [:pre {:style {:overflow-x "auto"}}
           (->> (music-theory/intervals-and-key-to-fretboard-matrix
                 music-theory/standard-tuning
                 key
                 intervals-xs
                 16)
                (music-theory/intervals-and-key-to-fretboard-matrix-str))]]

         [:h3 "Chords to scale"]
         (for [{chord-title :chord/title
                chord-id    :chord/id}
               (let [{scale-indexes :scale/indexes}
                     (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
                 (->> (vals @music-theory/chords-atom)
                      (filter (fn [{:chord/keys [indexes]}]
                                (set/subset? (set indexes) (set scale-indexes))))))]
           ^{:key chord-title}
           [:div {:style {:margin-right "10px" :display "inline"}}
            [:a {:href (rfe/href ::chord-tones {:chord-name chord-id :tone key})}
             [:button chord-title]]])]))))

(defn mode-view []
  (let [scale @(re-frame/subscribe [::scale])
        key   @(re-frame/subscribe [::key])]
    (when (and scale key)
      (let [{intervals     :scale/intervals
             scale-indexes :scale/indexes
             intervals-xs  :scale/intervals-xs
             sufix         :scale/sufix}
            (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
        [:div

         [:div
          (for [{:keys [tone url-name title]}
                (->> (music-theory/find-root-p :a)
                     (map (fn [x] {:tone     x
                                   :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                                   :title    (-> x tone->str)})))]
            ^{:key url-name}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::mode {:scale scale :key tone})}
              [:button
               {:disabled (= tone key)}
               title]]])]

         [:br]
         [:div
          (for [{scale' :scale
                 :keys [title]}
                (->> @music-theory/modes-atom
                     vals
                     (map (fn [{:mode/keys [scale] :as m}]
                            (merge m (get @music-theory/scales-atom scale))))
                     (map (fn [{scale :mode/scale title :scale/title}]
                            {:scale scale
                             :title title}))
                     (set)
                     (sort-by :title))]
            ^{:key (str title "-mode-select")}
            [:div {:style {:margin-right "10px" :display "inline"}}
             [:a {:href (rfe/href ::mode {:scale scale' :key key})}
              [:button
               {:disabled (= scale' scale)}
               title]]])]

         [:h2 (str (-> key tone->str) " - " (-> scale name str/capitalize))]
         [:pre {:style {:overflow-x "auto"}}
          (->> (map
                (fn [i x]
                  (str (fformat "%8s" i) " -> " (tone->str x)))
                intervals-xs
                ((get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale]) :scale/f])
                 (music-theory/find-root-p @(re-frame/subscribe [::key]))))
               (str/join "\n")
               (apply str)
               (str "Interval -> Tone\n"))]

         [:h3 "All tones in scale"]
         [:code
          [:pre {:style {:overflow-x "auto"}}
           (music-theory/fret-table-with-tones-p
            ((get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale]) :scale/f])
             (music-theory/find-root-p @(re-frame/subscribe [::key])))
            16)]]

         [:h3 "Mode patterns"]
         [:div
          (for [{mode-title     :mode/title
                 mode-pattern   :mode/pattern
                 root-on-string :mode/string
                 mode-id        :mode/id
                 scale-title    :scale/title}
                (->> @music-theory/modes-atom
                     vals
                     (sort-by :mode/string)
                     (map (fn [{:mode/keys [scale] :as m}]
                            (merge m (get @music-theory/scales-atom scale))))
                     (filter (comp #{scale} :mode/scale)))]
            ^{:key (str mode-title "-mode")}
            [:div
             (when root-on-string
               [:p (str "Root on string: " root-on-string)])
             [:code
              [:pre {:style {:overflow-x "auto"}}
               (music-theory/mode-pattern-str-p mode-id key)]]])]

         [:h3 "Chords to mode"]
         (for [{chord-title :chord/title
                chord-id    :chord/id}
               (let [{scale-indexes :scale/indexes}
                     (get-in @music-theory/scales-atom [@(re-frame/subscribe [::scale])])]
                 (->> (vals @music-theory/chords-atom)
                      (filter (fn [{:chord/keys [indexes]}]
                                (set/subset? (set indexes) (set scale-indexes))))))]
           ^{:key chord-title}
           [:div {:style {:margin-right "10px" :display "inline"}}
            [:a {:href (rfe/href ::chord-tones {:chord-name chord-id :tone key})}
             [:button chord-title]]])]))))

(defn download-object [export-name value]
  (let [data-blob (js/Blob. #js [value] #js {:type "plain/text"})
        link (.createElement js/document "a")]
    (set! (.-href link) (.createObjectURL js/URL data-blob))
    (.setAttribute link "download" export-name)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)))

(def neck-view-events-
  [{:n ::neck-view-all
    :s (fn [db [kw]] (get db kw true))}
   {:n ::neck-view-fulls
    :s (fn [db [kw]] (get db kw true))}
   {:n ::neck-view-halfs
    :s (fn [db [kw]] (get db kw true))}])

(doseq [{:keys [n s e]} neck-view-events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn the-neck-view []
  (let [all   @(re-frame/subscribe [::neck-view-all])
        fulls @(re-frame/subscribe [::neck-view-fulls])
        halfs @(re-frame/subscribe [::neck-view-halfs])]
    [:div
     [:h2 "Guitar neck with all tones in standard tuning."]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-all (not all)])} (if all "Hide" "Show")]
     (when all
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (music-theory/fret-table-with-tones-p music-theory/tones 25)]])
     [:br]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-fulls (not fulls)])} (if fulls "Hide" "Show")]
     (when fulls
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (music-theory/fret-table-with-tones-p
          (->> music-theory/tones
               (filter (comp not #(str/includes? % "#") name)))
          25)]])
     [:br]
     [:br]
     [:button {:on-click #(re-frame/dispatch [::neck-view-halfs (not halfs)])} (if halfs "Hide" "Show")]
     (when halfs
       [:code
        [:pre {:style {:overflow-x "auto"}}
         (music-theory/fret-table-with-tones-p
          (->> music-theory/tones
               (filter (comp not #(= % 1)count name)))
          25)]])]))

(def drill-events-
  [{:n ::tones-in-chord}
   {:n ::name-the-chord}
   {:n ::intervals-in-chord}
   {:n ::name-the-interval}
   {:n ::tone-in-interval}
   {:n ::modes-in-each-key}
   {:n ::find-all-locations-of-tone}
   {:n ::intervals-in-scales}])

(doseq [{:keys [n s e]} drill-events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))

(defn drills-view []
  (let [tones-in-chord?             @(re-frame/subscribe [::tones-in-chord])
        name-the-chord?             @(re-frame/subscribe [::name-the-chord])
        intervals-in-chord?         @(re-frame/subscribe [::intervals-in-chord])
        name-the-interval?          @(re-frame/subscribe [::name-the-interval])
        tone-in-interval?           @(re-frame/subscribe [::tone-in-interval])
        modes-in-each-key?          @(re-frame/subscribe [::modes-in-each-key])
        find-all-locations-of-tone? @(re-frame/subscribe [::find-all-locations-of-tone])
        intervals-in-scales?        @(re-frame/subscribe [::intervals-in-scales])]
    [:div
     [:div "Here you can generate and download music and guitar theory questions and
answers in an Org-drill format. Select the type of questions that interests you
and click on the download button. Open the downloaded file in Emacs and active
the org-drill mode."]
     [:p "Select questions:"]
     [:label "Tones in chord:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::tones-in-chord (not tones-in-chord?)])}]
     [:br]
     [:label "Name the chord:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::name-the-chord (not name-the-chord?)])}]
     [:br]
     [:label "Intervals in chord:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::intervals-in-chord (not intervals-in-chord?)])}]
     [:br]
     [:label "Name the interval:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::name-the-interval (not name-the-interval?)])}]
     [:br]
     [:label "Tone in interval:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::tone-in-interval (not tone-in-interval?)])}]
     [:br]
     [:label "Modes in each key:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::modes-in-each-key (not modes-in-each-key?)])}]
     [:br]
     [:label "Find locations of tone [C D E F G A B]:"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::find-all-locations-of-tone (not find-all-locations-of-tone?)])}]
     [:br]
     [:label "Intervals in scales"]
     [:input {:type "checkbox" :on-click #(re-frame/dispatch [::intervals-in-scales (not intervals-in-scales?)])}]
     [:br]
     [:br]
     [:br]
     [:button
      {:on-click
       (fn [_]
         (let [tones-in-chord-p
               (fn []
                 (drills/tones-in-chord music-theory/find-root-p music-theory/tones @music-theory/chords-atom))
               name-the-chord-p
               (fn []
                 (drills/name-the-chord music-theory/find-root-p music-theory/tones @music-theory/chords-atom))
               intervals-in-chord-p
               (fn []
                 (drills/intervals-in-chord music-theory/find-root-p music-theory/tones @music-theory/chords-atom))
               name-the-interval-p
               (fn []
                 (drills/name-the-interval music-theory/interval-p music-theory/find-root-p music-theory/tones @music-theory/chords-atom))
               tone-in-interval-p
               (fn []
                 (drills/tone-in-interval music-theory/interval-p music-theory/find-root-p music-theory/tones @music-theory/chords-atom))
               modes-in-each-key-p
               (fn []
                 (drills/modes-in-each-key music-theory/tones @music-theory/modes-atom music-theory/mode-pattern-str-p))
               find-all-locations-of-tone-p
               (fn []
                 (drills/find-all-locations-of-tone music-theory/tones music-theory/fret-table-with-tones-p))
               intervals-in-scales-p
               (fn []
                 (drills/intervals-in-scales @music-theory/scales-atom))]
           (download-object
            "music-theory-drills.org"
            (str
             "#+STARTUP: overview\n\n"
             (when tones-in-chord?             (tones-in-chord-p))
             (when name-the-chord?             (name-the-chord-p))
             (when intervals-in-chord?         (intervals-in-chord-p))
             (when name-the-interval?          (name-the-interval-p))
             (when tone-in-interval?           (tone-in-interval-p))
             (when modes-in-each-key?          (modes-in-each-key-p))
             (when find-all-locations-of-tone? (find-all-locations-of-tone-p))
             (when intervals-in-scales?        (intervals-in-scales-p))))))}
      "Download questions / answers"]]))

(def routes
  [v2-main/routes
   ["/"
    [""
     {:name      ::home
      :view      [home-page]
      :link-text "Home"}]
    ["the-neck"
     {:name ::the-neck
      :view [the-neck-view]}]
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
    ["drills"
     {:name      ::drills
      :view      [drills-view]
      :link-text "drills-view"}]
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
    ["mode/:scale/:key"
     {:name      ::mode
      :view      [mode-view]
      :link-text "mode"
      :controllers
      [{:parameters {:path [:scale :key]}
        :start      (fn [{{:keys [scale key]} :path}]
                      (let [scale' (keyword scale)
                            key'   (keyword key)]
                        (js/console.log "Entering mode:" scale key)
                        (re-frame/dispatch [::scale scale'])
                        (re-frame/dispatch [::key key'])))
        :stop       (fn [& params] (js/console.log "Leaving mode"))}]}]
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
        :stop       (fn [& params] (js/console.log "Leaving harmonization"))}]}]]])

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

(def debug? ^boolean goog.DEBUG)

(defn dev-setup []
  (when debug?
    (enable-console-print!)
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (rd/render
   [main router]
   (.getElementById js/document "app")))

(defn ^:dev/after-load init []
  (println "starting...")
  (re-frame/clear-subscription-cache!)
  (re-frame/dispatch-sync [::initialize-db])
  (dev-setup)
  (init-routes!) ;; Reset routes on figwheel reload
  (mount-root))
