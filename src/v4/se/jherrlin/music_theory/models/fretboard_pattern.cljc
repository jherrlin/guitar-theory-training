(ns v4.se.jherrlin.music-theory.models.fretboard-pattern
  "FretboardPattern

  {:fretboard-pattern/id         :major-1,
   :fretboard-pattern/type       :triad
   :fretboard-pattern/chord-id   :major,
   :fretboard-pattern/tuning     [:e :b :g :d :a :e],
   :fretboard-pattern/inversion? false,
   :fretboard-pattern/pattern
     [[\"3\" nil nil nil]
      [nil \"1\" nil nil]
      [\"5\" nil nil nil]
      [nil nil \"3\" nil]
      [nil nil nil \"1\"]
      [nil nil nil nil]],
   :fretboard-pattern/str
     (str
      \"3   -   -   -\\n\"
      \"-   1   -   -\\n\"
      \"5   -   -   -\\n\"
      \"-   -   3   -\\n\"
      \"-   -   -   1\\n\"
      \"-   -   -   -\")}"
  (:require
   [malli.core :as m]))


(def FretboardPattern
  [:map
   [:fretboard-pattern/id         keyword?]
   [:fretboard-pattern/type       [:enum :triad :mode :scale]]
   [:fretboard-pattern/chord-id   {:optional true} keyword?]
   [:fretboard-pattern/tuning     [:+ keyword?]]
   [:fretboard-pattern/pattern    [:vector [:+ [:alt string? nil?]]]]
   [:fretboard-pattern/str        string?]
   [:fretboard-pattern/inversion? boolean?]])

(def FretboardPatterns
  [:map-of :keyword FretboardPattern])


(def validate-fretboard-pattern?  (partial m/validate FretboardPattern))
(def validate-fretboard-patterns? (partial m/validate FretboardPatterns))
(def explain-fretboard-pattern    (partial m/explain  FretboardPattern))
(def explain-fretboard-patterns   (partial m/explain  FretboardPatterns))


(comment

  (m/validate [:+ keyword?] [:e :b :g :d :a :e])

  (m/validate [:vector [:+ [:alt string? nil?]]] [["3" nil nil nil]])

  (def fretboard-pattern
    {:fretboard-pattern/id         :major-1,
     :fretboard-pattern/type       :triad
     :fretboard-pattern/chord-id   :major,
     :fretboard-pattern/tuning     [:e :b :g :d :a :e],
     :fretboard-pattern/inversion? false,
     :fretboard-pattern/pattern
     [["3" nil nil nil]
      [nil "1" nil nil]
      ["5" nil nil nil]
      [nil nil "3" nil]
      [nil nil nil "1"]
      [nil nil nil nil]],
     :fretboard-pattern/str
     (str
      "3   -   -   -\n"
      "-   1   -   -\n"
      "5   -   -   -\n"
      "-   -   3   -\n"
      "-   -   -   1\n"
      "-   -   -   -")})

  (m/validate FretboardPattern  fretboard-pattern)
  (m/validate FretboardPatterns {:major-1 fretboard-pattern})
  (explain-fretboard-pattern    fretboard-pattern)
  )
