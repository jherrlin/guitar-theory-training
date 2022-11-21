(ns v4.se.jherrlin.music-theory.models.scale
  (:require
   [malli.core :as m]
   [v4.se.jherrlin.music-theory.models.chord :as chord]))


(def regex chord/regex)


(def Scale
  "Scale model.

  Defines a scale without notion of instrument."
  [:map
   [:scale/id         keyword?]
   [:scale/intervals  [:vector string?]]
   [:scale/indexes    [:vector number?]]
   [:scale/name       string?]
   [:scale/text       {:optional true} string?]])

(def Scales
  [:map-of :keyword Scale])


(def valid-scale?   (partial m/validate Scale))
(def valid-scales?  (partial m/validate Scales))
(def explain-scale  (partial m/explain  Scale))
(def explain-scales (partial m/explain  Scales))


(comment

  (valid-scale?
   #:scale{:id :ionian,
           :intervals ["1" "2" "3" "4" "5" "6" "7"],
           :indexes [0 2 4 5 7 9 11],
           :name "ionian",
           :order 4})


  (valid-scales?
   {:ionian
    #:scale{:id :ionian,
            :intervals ["1" "2" "3" "4" "5" "6" "7"],
            :indexes [0 2 4 5 7 9 11],
            :name "ionian",
            :order 4}})

  )
