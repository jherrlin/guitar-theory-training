(ns v5.se.jherrlin.music-theory.models.fretboard-pattern
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
   [malli.core :as m]
   [v5.se.jherrlin.music-theory.models.chord :as chord]))

(def regex
  "Regex to find fretboard patterns from the DSL

  \"3   -   -   -\"
  =>
  ([\"3\" \"3\"] [\"-\" nil] [\"-\" nil] [\"-\" nil])"
  (str "(" chord/regex ")|-"))

(def FretboardPattern
  [:map
   [:fretboard-pattern/id         keyword?]
   [:fretboard-pattern/type       [:enum :triad :mode :scale :chord]]
   [:fretboard-pattern/tuning     [:+ keyword?]]
   [:fretboard-pattern/pattern    [:vector [:+ [:alt string? nil?]]]]
   [:fretboard-pattern/on-strings {:optional true} [:vector int?]]
   [:fretboard-pattern/str        string?]
   [:fretboard-pattern/inversion? {:optional true} boolean?]])

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

  (re-seq (re-pattern regex) "5   -   -   -")

  (re-seq
   #"(b{0,2}#{0,2}\d{1,2})|-"
   "5   -   -   -")

  (->> "5   -   -   -"
       (re-seq (re-pattern regex))
       (mapv second)))
