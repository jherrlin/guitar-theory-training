(ns v2.se.jherrlin.music-theory.intervals)

(def perfect-unison     0)
(def root               0)
(def minor-second       1)
(def major-second       2)
(def augmented-second   3)
(def minor-third        3)
(def major-third        4)
(def augmented-third    5)
(def diminished-fourth  4)
(def perfect-fourth     5)
(def augmented-fourth   6)
(def diminished-fifth   6)
(def perfect-fifth      7)
(def augmented-fifth    8)
(def minor-sixth        8)
(def major-sixth        9)
(def augmented-sixth    10)
(def diminished-seventh 9)
(def minor-seventh      10)
(def major-seventh      11)
(def octave             perfect-unison)
(def perfect-octave     perfect-unison)

(defn intervals []
  [{:semitones perfect-unison
    :function  "1"
    :name      "Perfect unison"}
   {:semitones minor-second
    :function  "b2"
    :name      "Minor second"}
   {:semitones major-second
    :function  "2"
    :name      "Major second"}
   {:semitones augmented-second
    :function  "#2"
    :name      "Augmented second"}
   {:semitones minor-third
    :function  "b3"
    :name      "Minor third"}
   {:semitones major-third
    :function  "3"
    :name      "Major third"}
   {:semitones augmented-third
    :function  "#3"
    :name      "Augmented third"}
   {:semitones diminished-fourth
    :function  "b4"
    :name      "Diminished fourth"}
   {:semitones perfect-fourth
    :function  "4"
    :name      "perfect fourth"}
   {:semitones augmented-fourth
    :function  "#4"
    :name      "Augmented fourth"}
   {:semitones diminished-fifth
    :function  "b5"
    :name      "Diminished fifth"}
   {:semitones perfect-fifth
    :function  "5"
    :name      "Perfect fifth"}
   {:semitones augmented-fifth
    :function  "#5"
    :name      "Augmented fifth"}
   {:semitones minor-sixth
    :function  "b6"
    :name      "Minor sixth"}
   {:semitones major-sixth
    :function  "6"
    :name      "Major sixth"}
   {:semitones augmented-sixth
    :function  "#6"
    :name      "Augmented sixth"}
   {:semitones diminished-seventh
    :function  "bb7"
    :name      "Diminished seventh"}
   {:semitones minor-seventh
    :function  "b7"
    :name      "Minor seventh"}
   {:semitones major-seventh
    :function  "7"
    :name      "Major seventh"}])

(def intervals-map-by-function
  (->> (intervals)
       (map (juxt :function identity))
       (into {})))

(comment
  intervals-map-by-function
  )
