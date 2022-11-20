(ns v4.se.jherrlin.music-theory.models.chord
  "Chord

  {:chord/id           :major,
   :chord/intervals    [\"1\" \"3\" \"5\"],
   :chord/indexes      [0 4 7],
   :chord/title        \"major\",
   :chord/sufix        \"\"}"
  (:require [malli.core :as m]))


(def Chord
  [:map
   [:chord/id          keyword?]
   [:chord/intervals   [:vector string?]]
   [:chord/indexes     [:vector number?]]
   [:chord/title       string?]
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
   {:chord/id           :major,
    :chord/intervals    ["1" "3" "5"],
    :chord/indexes      [0 4 7],
    :chord/title        "major",
    :chord/sufix        ""})

  (m/validate
   Chords
   {:major
    {:chord/id           :major,
     :chord/intervals    ["1" "3" "5"],
     :chord/indexes      [0 4 7],
     :chord/title        "major",
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
     :chord/title        "major",
     :chord/order        1,
     :chord/sufix        "",
     :chord/explanation  "major",
     :chord/display-text "major"}})
  )
