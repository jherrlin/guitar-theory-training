(ns se.jherrlin.drills.scales
  (:require
   [se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]
   [clojure.string :as str]))


(defn intervals-in-scales [scales-map]
  (let [
        ;; scales-map @se.jherrlin.music-theory/scales-atom
        ]
    (->> scales-map
         vals
         (map (fn [{:scale/keys [intervals-xs title]}]
                (str
                 "** " (fformat "%-60s:music:theory:scales:drill:" (str "Intervals in " title " scale"))
                 "\n\n"
                 "   " "What are the intervals in " title " scale?"
                 "\n\n"
                 "*** Answer "
                 "\n\n    " (str/join ", " intervals-xs)
                 "\n\n")))
         (apply str))))
