(ns se.jherrlin.drills.scales
  (:require
   [clojure.string :as str]
   [se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(defn intervals-in-scales [scales-map]
  (let [;; scales-map @v2.se.jherrlin.music-theory.definitions/scales-atom
        ]
    (->> scales-map
         vals
         (map (fn [{:scale/keys [intervals title]}]
                (str
                 "** " (fformat "%-60s:music:theory:scales:drill:" (str "Intervals in " title " scale"))
                 "\n\n"
                 "   " "What are the intervals in " title " scale?"
                 "\n\n"
                 "*** Answer "
                 "\n\n    " (str/join ", " intervals)
                 "\n\n")))
         (apply str))))
