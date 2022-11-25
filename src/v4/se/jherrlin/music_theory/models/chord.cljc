(ns v4.se.jherrlin.music-theory.models.chord
  "Chord

  {:chord/id           :major,
   :chord/intervals    [\"1\" \"3\" \"5\"],
   :chord/indexes      [0 4 7],
   :chord/name        \"major\",
   :chord/sufix        \"\"}"
  (:require [malli.core :as m]))



(def regex
  "Pattern to find intervals in text.

  \"1, b3 b5 bb7\"
  =>
  (\"1\" \"b3\" \"b5\" \"bb7\")"
  "b{0,2}#{0,2}\\d{1,2}")


(def Chord
  "Chord model.

  Defines a chord without notion of instrument."
  [:map
   [:chord/id          keyword?]
   [:chord/intervals   [:vector string?]]
   [:chord/indexes     [:vector number?]]
   [:chord/name       string?]
   [:chord/sufix       string?]
   [:chord/explanation {:optional true} string?]])

(def Chords
  [:map-of :keyword Chord])


(def valid-chord?   (partial m/validate Chord))
(def valid-chords?  (partial m/validate Chords))
(def explain-chord  (partial m/explain  Chord))
(def explain-chords (partial m/explain  Chords))


(comment
  (m/schema Chord)
  (m/schema? (m/schema Chord))

  (m/validate
   Chord
   {:chord/id        :major,
    :chord/intervals ["1" "3" "5"],
    :chord/indexes   [0 4 7],
    :chord/name     "major",
    :chord/sufix     ""})

  (m/validate
   Chords
   {:major
    {:chord/id           :major,
     :chord/intervals    ["1" "3" "5"],
     :chord/indexes      [0 4 7],
     :chord/name        "major",
     :chord/order        1,
     :chord/sufix        "",
     :chord/explanation  "major",
     :chord/display-text "major"}})

  (m/explain
   Chords
   {:major
    {:chord/id           :major,
     :chord/intervals    ["1" "3" "5"],
     ;; :chord/indexes      [0 4 7],
     :chord/name        "major",
     :chord/order        1,
     :chord/sufix        "",
     :chord/explanation  "major",
     :chord/display-text "major"}})

  (re-seq (re-pattern regex) "1 3 #5")
  (re-seq (re-pattern regex) "1 b3 b5 bb7")
  (re-seq (re-pattern regex) "1, 3,  #5")
  (re-seq (re-pattern regex) "1, b3 b5 bb7")


  (defn derp [xs]
    {:malli/schema [:=> [:cat Chords] Chord]}
    (->> xs vals first))

  (m/=> derp [:=> [:cat Chords] Chord])

  (derp "")

  ((requiring-resolve 'malli.clj-kondo/emit!))

  )
