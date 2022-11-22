(ns v4.se.jherrlin.music-theory.intervals)


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
(def ninth              major-second)
(def eleventh           5)
(def thirteen           8)
(def octave             perfect-unison)
(def perfect-octave     perfect-unison)

(defn intervals []
  [{:semitones perfect-unison
    :function  "1"
    :name/en   "Root"
    :name/sv   "Root"}
   {:semitones minor-second
    :function  "b2"
    :name/en   "Minor second"
    :name/sv   "Liten sekund"}
   {:semitones major-second
    :function  "2"
    :name/en   "Major second"
    :name/sv   "Sekund"}
   {:semitones augmented-second
    :function  "#2"
    :name/en   "Augmented second"
    :name/sv   "Augmented second"}
   {:semitones minor-third
    :function  "b3"
    :name/en   "Minor third"
    :name/sv   "Moll-ters"}
   {:semitones major-third
    :function  "3"
    :name/en   "Major third"
    :name/sv   "Dur-ters"}
   {:semitones augmented-third
    :function  "#3"
    :name/en   "Augmented third"
    :name/sv   "Augmented third"}
   {:semitones diminished-fourth
    :function  "b4"
    :name/en   "Diminished fourth"
    :name/sv   "Diminished fourth"}
   {:semitones perfect-fourth
    :function  "4"
    :name/en   "perfect fourth"
    :name/sv   "Kvart"}
   {:semitones augmented-fourth
    :function  "#4"
    :name/en   "Augmented fourth"
    :name/sv   "Augmented fourth"}
   {:semitones diminished-fifth
    :function  "b5"
    :name/en   "Diminished fifth"
    :name/sv   "Diminished fifth"}
   {:semitones perfect-fifth
    :function  "5"
    :name/en   "Perfect fifth"
    :name/sv   "Kvint"}
   {:semitones augmented-fifth
    :function  "#5"
    :name/en   "Augmented fifth"
    :name/sv   "Augmented fifth"}
   {:semitones minor-sixth
    :function  "b6"
    :name/en   "Minor sixth"
    :name/sv   "Liten sexa"}
   {:semitones major-sixth
    :function  "6"
    :name/en   "Major sixth"
    :name/sv   "Stor sexa"}
   {:semitones augmented-sixth
    :function  "#6"
    :name/en   "Augmented sixth"
    :name/sv   "Augmented sixth"}
   {:semitones diminished-seventh
    :function  "bb7"
    :name/en   "Diminished seventh"
    :name/sv   "Dubbelsänkt sjua"}
   {:semitones minor-seventh
    :function  "b7"
    :name/en   "Minor seventh"
    :name/sv   "Liten sjua"}
   {:semitones major-seventh
    :function  "7"
    :name/en   "Major seventh"
    :name/sv   "Sjua"}
   {:semitones ninth
    :function  "9"
    :name/en   "Ninth"
    :name/sv   "Nia"}
   {:semitones eleventh
    :function  "11"
    :name/en   "Eleventh"
    :name/sv   "Elva"}
   {:semitones thirteen
    :function  "13"
    :name/en   "Thirteen"
    :name/sv   "Tretton"}])

(def intervals-map-by-function
  (->> (intervals)
       (map (juxt :function identity))
       (into {})))

(defn functions-to-semitones [functions]
  (->> functions
       (mapv #(get-in intervals-map-by-function [% :semitones]))))

(comment
  intervals-map-by-function
  )
