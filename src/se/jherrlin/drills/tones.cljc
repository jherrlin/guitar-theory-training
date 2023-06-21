(ns se.jherrlin.drills.tones
  (:require
   [clojure.string :as str]
   [se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(defn find-all-locations-of-tone [all-tones fret-table-with-tones-f]
  (let [;; all-tones               se.jherrlin.music-theory/tones
        ;; fret-table-with-tones-f se.jherrlin.music-theory/fret-table-with-tones-p
        ]
    (->> all-tones
         (filter (comp #(not (str/includes? % "#")) name))
         vec
         (map (fn [tone]
                (let [tone-str    (-> tone name str/upper-case)
                      nr-of-frets 13]
                  (str
                   "** " (fformat "%-60s:music:theory:tonelocation:drill:" (str "Tone " tone-str " locations on the fretboard"))
                   "\n\n"
                   "   " "Find all locations on the fretboard where tone " tone-str " is located."
                   "\n\n"
                   "   " "The fretboard is " nr-of-frets " frets long."
                   "\n\n"
                   "*** Answer "
                   "\n\n"
                   (fret-table-with-tones-f [tone] nr-of-frets)
                   "\n"))))
         (apply str))))
