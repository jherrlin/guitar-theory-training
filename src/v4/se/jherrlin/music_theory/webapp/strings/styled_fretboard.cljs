(ns v4.se.jherrlin.music-theory.webapp.strings.styled-fretboard
  (:require
   [re-frame.core :as re-frame]))


#_(def matrix
  [[{:x 0, :tone #{:e}, :y 0, :out "E"}
    {:x 1, :tone #{:f}, :y 0}
    {:x 2, :tone #{:gb :f#}, :y 0}
    {:x 3, :tone #{:g}, :y 0, :out "G"}
    {:x 4, :tone #{:g# :ab}, :y 0}
    {:x 5, :tone #{:a}, :y 0}
    {:x 6, :tone #{:bb :a#}, :y 0}
    {:x 7, :tone #{:b}, :y 0, :out "B"}
    {:x 8, :tone #{:c}, :y 0}
    {:x 9, :tone #{:db :c#}, :y 0}]
   [{:x 0, :tone #{:b}, :y 1, :out "B"}
    {:x 1, :tone #{:c}, :y 1}
    {:x 2, :tone #{:db :c#}, :y 1}
    {:x 3, :tone #{:d}, :y 1}
    {:x 4, :tone #{:d# :eb}, :y 1}
    {:x 5, :tone #{:e}, :y 1, :out "E"}
    {:x 6, :tone #{:f}, :y 1}
    {:x 7, :tone #{:gb :f#}, :y 1}
    {:x 8, :tone #{:g}, :y 1, :out "G"}
    {:x 9, :tone #{:g# :ab}, :y 1}]
   [{:x 0, :tone #{:g}, :y 2, :out "G"}
    {:x 1, :tone #{:g# :ab}, :y 2}
    {:x 2, :tone #{:a}, :y 2}
    {:x 3, :tone #{:bb :a#}, :y 2}
    {:x 4, :tone #{:b}, :y 2, :out "B"}
    {:x 5, :tone #{:c}, :y 2}
    {:x 6, :tone #{:db :c#}, :y 2}
    {:x 7, :tone #{:d}, :y 2}
    {:x 8, :tone #{:d# :eb}, :y 2}
    {:x 9, :tone #{:e}, :y 2, :out "E"}]
   [{:x 0, :tone #{:d}, :y 3}
    {:x 1, :tone #{:d# :eb}, :y 3}
    {:x 2, :tone #{:e}, :y 3, :out "E"}
    {:x 3, :tone #{:f}, :y 3}
    {:x 4, :tone #{:gb :f#}, :y 3}
    {:x 5, :tone #{:g}, :y 3, :out "G"}
    {:x 6, :tone #{:g# :ab}, :y 3}
    {:x 7, :tone #{:a}, :y 3}
    {:x 8, :tone #{:bb :a#}, :y 3}
    {:x 9, :tone #{:b}, :y 3, :out "B"}]
   [{:x 0, :tone #{:a}, :y 4}
    {:x 1, :tone #{:bb :a#}, :y 4}
    {:x 2, :tone #{:b}, :y 4, :out "B"}
    {:x 3, :tone #{:c}, :y 4}
    {:x 4, :tone #{:db :c#}, :y 4}
    {:x 5, :tone #{:d}, :y 4}
    {:x 6, :tone #{:d# :eb}, :y 4}
    {:x 7, :tone #{:e}, :y 4, :out "E"}
    {:x 8, :tone #{:f}, :y 4}
    {:x 9, :tone #{:gb :f#}, :y 4}]
   [{:x 0, :tone #{:e}, :y 5, :out "E"}
    {:x 1, :tone #{:f}, :y 5}
    {:x 2, :tone #{:gb :f#}, :y 5}
    {:x 3, :tone #{:g}, :y 5, :out "G"}
    {:x 4, :tone #{:g# :ab}, :y 5}
    {:x 5, :tone #{:a}, :y 5}
    {:x 6, :tone #{:bb :a#}, :y 5}
    {:x 7, :tone #{:b}, :y 5, :out "B"}
    {:x 8, :tone #{:c}, :y 5}
    {:x 9, :tone #{:db :c#}, :y 5}]])



(defn styled-view [matrix]
  (let [circle-comp (fn [color] {:display          :flex
                                 :height           "2em"
                                 :width            "2em"
                                 :background-color color #_ "#ff7400"
                                 :border-radius    "50%"
                                 :z-index          0
                                 :align-items      :center
                                 :justify-content  :center})
        max-x       (->> matrix first (map :x) (apply max))]
    [:div
     [:div {:style {:display "flex"}}
      (for [{:keys [x]} (-> matrix first)]
        ^{:key (str "fretboard-fret-" x)}
        [:div {:style {:width           "6.4em"
                       :display         "flex"
                       :justify-content "center"}}
         x])]
     (for [[idx fretboard-string] (map-indexed vector matrix)]
       ^{:key (str "fretboard-string-" idx)}
       [:div {:style {:display "flex"}}
        (for [{:keys [x y tone out]} fretboard-string]
          ^{:key (str "fretboard-string-" x "-" y)}
          [:<>
           [:div {:style {:display          "flex"
                          :flex-direction   "column"
                          :height           "3em"
                          :width            "6em"
                          :background-color (when-not (= x 0) "#000000d6")
                          :justify-content  "center"}}
            [:div {:style {:background-image "linear-gradient(#737270 , #b9bab3, #737270)"
                           :display          "flex"
                           :margin-left      (when-not (= 0 x) "-0.5em")
                           :margin-right     (when-not (= x max-x) "-0.5em")
                           :margin-bottom    "-0.5em"
                           :height           (str (+ 3 idx) "px")}}]
            [:div {:style {:display         "flex"
                           :align-items     "center"
                           :justify-content "center"
                           :height          (str (+ 3 idx) "px")}}
             [:div {:style (when out
                             (circle-comp "orange"))}
              (when out out)]]]
           (when-not (= x max-x)
             [:div {:style {:background-image (if (= x 0)
                                                "linear-gradient(to right, black , black)"
                                                "linear-gradient(to right, #ad9336 , #706e68)")
                            :width            "6px"
                            :height           "3em"}}])])])]))

#_(def routes
  (let [path-name :a/a]
    ["/v5/derp"
     {:name path-name
      :view [styled-view]
      :controllers
      [{:parameters {:path  params/path-params
                     :query params/query-params}
        :start      (fn [{p :path q :query}]
                      (re-frame/dispatch [:path-params p])
                      (re-frame/dispatch [:query-params q])
                      (re-frame/dispatch [:path-name path-name]))}]}]))
