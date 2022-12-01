(ns v4.se.jherrlin.music-theory.webapp.piano.common
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [se.jherrlin.music-theory :as music-theory]
   [v4.se.jherrlin.music-theory.utils :as utils]
   [se.jherrlin.utils
    :refer [fformat rotate-until]]))


(defn links-to-chords [chords chord href-fn]
  [:div
   (for [{id           :chord/id
          sufix        :chord/sufix
          display-text :chord/display-text}
         (->> chords
              vals
              (sort-by :chord/order))]
     ^{:key (str id sufix "-chord")}
     [:div {:style {:margin-right "10px" :display "inline"}}
      [:a {:href (href-fn id)}
       [:button
        {:disabled (= id chord)}
        (str (or display-text sufix))]]])])

(defn debug-view
  ([]
   (debug-view @re-frame.db/app-db))
  ([x]
   [:pre
    (with-out-str (cljs.pprint/pprint x))]))


(defn links-to-scales [scales scale href-fn]
  [:div
   (for [{id         :scale/id
          scale-name :scale/name}
         (->> scales
              vals
              (sort-by :scale/order))]
     ^{:key (str id scale-name "-scale")}
     [:div {:style {:margin-right "10px" :display "inline"}}
      [:a {:href (href-fn id)}
       [:button
        {:disabled (= id scale)}
        (str scale-name)]]])])

(defn links-to-keys [key-of href-fn]
  [:div
   (for [{tone' :tone
          :keys [url-name title]}
         (->> (music-theory/find-root-p :a)
              (map (fn [x] {:tone     x
                            :url-name (-> x name str/lower-case (str/replace "#" "sharp"))
                            :title    (-> x name str/capitalize)})))]
     ^{:key url-name}
     [:div {:style {:margin-right "10px" :display "inline"}}
      [:a {:href (href-fn tone')}
       [:button
        {:disabled (= key-of tone')}
        title]]])])

(defn highlight-tones [tones key-of]
  [:div {:style {:margin-top  "1em"
                        :display     "flex"
                        :align-items "center"}}

          (for [{:keys [tone match?]}
                (let [tones-set (set tones)]
                  (->> (utils/all-tones)
                       (rotate-until #(% key-of))
                       (map (fn [tone]
                              (cond-> {:tone tone}
                                (seq (set/intersection tones-set tone))
                                (assoc :match? true))))))]
            ^{:key (str tone "something")}
            [:div {:style {:width     "4.5em"
                           :font-size "0.9em"}}
             (for [t' (sort-by (fn [x]
                                 (let [x (str x)]
                                   (cond
                                     (and (= (count x) 3) (str/includes? x "#"))
                                     1
                                     (= (count x) 3)
                                     2
                                     :else 0))) tone)]
               ^{:key (str tone t')}
               [:div {:style {:margin-top "0em"}}
                [:div
                 {:style {:color       (if-not match? "grey")
                          :font-weight (if match? "bold")}}
                 (-> t' name str/capitalize)]])])])

(defn intervals-to-tones [intervals tones]
  [:pre {:style {:overflow-x "auto"}}
   (->> (map
         (fn [interval index]
           (str (fformat "%8s" interval) " -> " (-> index name str/capitalize)))
         intervals
         tones)
        (str/join "\n")
        (apply str)
        (str "Interval -> Tone\n"))])

(defn chord-name
  [key-of
   {explanation :chord/explanation
    sufix       :chord/sufix}]
  [:div {:style {:margin-top "1em"
                 :height     "100%"
                 :display    "inline-flex"}}
          [:h2 (str (-> key-of name str/capitalize) sufix)]
          (when explanation
            [:p {:style {:margin-left "4em"
                         :margin-top  "0.5em"}}
             (str "(" explanation ")")])])
