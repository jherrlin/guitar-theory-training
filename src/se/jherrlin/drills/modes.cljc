(ns se.jherrlin.drills.modes
  (:require
   [clojure.string :as str]
   [se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(defn modes-in-each-key [all-tones modes-map mode-pattern-str-f]
  (let [;; all-tones          #_se.jherrlin.music-theory/tones [:c :e]
        ;; modes-map          @se.jherrlin.music-theory/modes-atom
        ;; mode-pattern-str-f se.jherrlin.music-theory/mode-pattern-str-p
        ]
    (let [mode-patterns-map
          (->> modes-map
               vals
               (group-by :mode/scale))]
      (->> (for [mode-scale (->> modes-map
                                 vals
                                 (map :mode/scale)
                                 (set))
                 tone       all-tones]
             (let [mode-patterns (get mode-patterns-map mode-scale)]

               (let [{:mode/keys [scale pattern id title pattern-str]}
                     (first mode-patterns)
                     tone-str  (-> tone name str/upper-case)
                     scale-str (-> scale name (str/replace "-" " ") str/capitalize)]
                 (str
                  "** " (fformat "%-60s:music:theory:chords:drill:" (str "Modes in " tone-str " " scale-str))
                  "\n\n"
                  "   Play all the modes in " tone-str " " scale-str "."
                  "\n\n"
                  "*** Answer \n\n"
                  (->> mode-patterns
                       (sort-by :mode/string)
                       (map :mode/id)
                       (set)
                       (map (fn [id]
                              (str
                               (mode-pattern-str-f id tone)
                               "\n\n")))
                       (apply str))))))
           (apply str)))))

#?(:clj
   (comment
     (modes-in-each-key
      se.jherrlin.music-theory/tones @se.jherrlin.music-theory/modes-atom se.jherrlin.music-theory/mode-pattern-str-p)))
