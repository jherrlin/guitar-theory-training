(ns v5.se.jherrlin.music-theory.models.scale
  (:require
   [malli.core :as m]))


(def Scale
  "Scale model.

  Defines a scale without notion of instrument."
  [:map
   [:id               uuid?]
   [:scale/scale      keyword?]
   [:scale/intervals  [:vector string?]]
   [:scale/indexes    [:vector number?]]
   [:scale/name       string?]
   [:scale/text       {:optional true} string?]])

(def Scales
  [:map-of :uuid Scale])

(def valid-scale?   (partial m/validate Scale))
(def valid-scales?  (partial m/validate Scales))
(def explain-scale  (partial m/explain  Scale))
(def explain-scales (partial m/explain  Scales))

(comment

  (valid-scale?
   {:id              #uuid "e6ff98f9-59a9-4421-9bbc-491eadae8587"
    :scale/scale     :ionian,
    :scale/intervals ["1" "2" "3" "4" "5" "6" "7"],
    :scale/indexes   [0 2 4 5 7 9 11],
    :scale/name      "ionian",
    :scale/order     4})

  (valid-scales?
   {(random-uuid)
    {:id              #uuid "e6ff98f9-59a9-4421-9bbc-491eadae8587"
     :scale/scale     :ionian,
     :scale/intervals ["1" "2" "3" "4" "5" "6" "7"],
     :scale/indexes   [0 2 4 5 7 9 11],
     :scale/name      "ionian",
     :scale/order     4}})
  )
