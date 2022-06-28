(ns se.jherrlin.drills.chords
  (:require
   [se.jherrlin.music-theory.utils
    :refer [fformat find-chord find-chord-name  find-root
            fret-table-with-tones match-chord-with-scales]
    :as utils]
   [clojure.string :as str])
  )


(defn tones-in-chords [find-root-f all-tones chords-map]
  (let [
        ;; find-root-f        se.jherrlin.music-theory/find-root-p
        ;; all-tones          se.jherrlin.music-theory/tones
        ;; chords-map         @se.jherrlin.music-theory/chords-atom
        ]
    (->> (for [chord (vals chords-map)
               tone  all-tones]
         (assoc chord :tone tone))
         (map (fn [{:chord/keys [f sufix]
                    tone        :tone}]
                (let [chord-name      (str (-> tone name str/upper-case) sufix)
                      chord-tones     (f (find-root-f tone))
                      chord-tones-str (->> chord-tones (map (comp str/upper-case name)) (str/join ", "))]
                (str
                 "** " (fformat "%-60s:music:theory:chords:drill:" (str "Tones in " chord-name " chord"))
                 "\n\n"
                 "   What tones are in the " chord-name " chord?"
                 "\n\n"
                 "*** Answer \n\n    " chord-tones-str
                 ;; (fret-board-table-f chord-tones)
                 "\n\n"))))
         (apply str))))

{:chord/intervals    "1 b3 b5 bb7",
 :chord/sufix        "dim7",
 :chord/title        "diminished seventh",
 :chord/tags         #{:diminished :seventh :minor},
 :tone               :c,
 ;; :chord/f            #function[clojure.core/juxt/fn--5895],
 :chord/intervals-xs ["1" "b3" "b5" "bb7"],
 :chord/id           :diminished-seventh,
 :chord/explanation  "diminished seven",
 :chord/indexes      [0 3 6 9]}

#?(:clj
   (comment
     (spit
      "/tmp/tones-in-chords.org"
      (with-out-str
        (print
         (tones-in-chords
          ;; se.jherrlin.music-theory/fret-table-with-tones-p
          se.jherrlin.music-theory/find-root-p
          se.jherrlin.music-theory/tones
          @se.jherrlin.music-theory/chords-atom)))))
   )


(defn name-the-chord [find-root-f all-tones chords-map]
  (let [
        ;; find-root-f        se.jherrlin.music-theory/find-root-p
        ;; all-tones          se.jherrlin.music-theory/tones
        ;; chords-map         @se.jherrlin.music-theory/chords-atom
        ]
    (->> (for [chord (vals chords-map)
               tone  all-tones]
           (assoc chord :tone tone))
         (map (fn [{:chord/keys [f sufix]
                    tone        :tone}]
                (let [chord-name      (str (-> tone name str/upper-case) sufix)
                      chord-tones     (f (find-root-f tone))
                      chord-tones-str (->> chord-tones (map (comp str/upper-case name)) (str/join ", "))]
                  (str
                   "** " (fformat "%-60s:music:theory:chords:drill:" (str "Name the chord " chord-tones-str))
                   "\n\n"
                   "   Name the chord with the following tones: " chord-tones-str
                   "\n\n"
                   "*** Answer \n\n    " chord-name
                   "\n\n"))))
         (apply str))))

#?(:clj
   (comment
     (spit
      "/tmp/name-the-chord.org"
      (with-out-str
        (print
         (name-the-chord
          ;; se.jherrlin.music-theory/fret-table-with-tones-p
          se.jherrlin.music-theory/find-root-p
          se.jherrlin.music-theory/tones
          @se.jherrlin.music-theory/chords-atom))))
     ))


(defn intervals-in-chord [find-root-f all-tones chords-map]
  (let [
        ;; find-root-f        se.jherrlin.music-theory/find-root-p
        ;; all-tones          se.jherrlin.music-theory/tones
        ;; chords-map         @se.jherrlin.music-theory/chords-atom
        ]
    (->> (for [chord (vals chords-map)
               tone  all-tones]
           (assoc chord :tone tone))
         (map (fn [{:chord/keys [f sufix intervals]
                    tone        :tone}]
                (let [chord-name      (str (-> tone name str/upper-case) sufix)
                      chord-tones     (f (find-root-f tone))
                      chord-tones-str (->> chord-tones (map (comp str/upper-case name)) (str/join ", "))]
                  (str
                   "** " (fformat "%-60s:music:theory:chords:drill:" (str "Intervals in " chord-name))
                   "\n\n"
                   "   What intervals are in the " chord-name " chord?"
                   "\n\n"
                   "*** Answer \n\n    " intervals
                   "\n\n"))))
         (apply str))))
