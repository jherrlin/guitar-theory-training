(ns se.jherrlin.drills.intervals
  (:require
   [clojure.string :as str]
   [se.jherrlin.music-theory.intervals :refer [intervals]]
   [se.jherrlin.music-theory.utils
    :refer [fformat]
    :as utils]))

(defn name-the-interval [interval-f find-root-f all-tones chords-map]
  (let [;; interval-f  se.jherrlin.music-theory/interval-p
        ;; find-root-f se.jherrlin.music-theory/find-root-p
        ;; all-tones   se.jherrlin.music-theory/tones
        ;; chords-map  @se.jherrlin.music-theory/chords-atom
        ]
    (->> (for [{:keys [semitones] :as interval} (intervals)
               tone                             all-tones]
           (assoc interval :start-tone tone :end-tone (interval-f tone semitones)))
         (map (fn [{interval-name :name
                    :keys         [semitones function start-tone end-tone]}]
                (let [start-tone' (-> start-tone name str/upper-case)
                      end-tone'   (-> end-tone name str/upper-case)]
                  (str
                   "** " (fformat "%-60s:music:theory:intervals:drill:" (str "Name the interval and function from " start-tone' " to " end-tone'))
                   "\n\n"
                   "   " "Name the interval from " start-tone' " to " end-tone'
                   "\n\n"
                   "*** Answer "
                   "\n\n   Interval name: " interval-name
                   "\n   Function: " function
                   "\n\n"))))
         (apply str))))

#?(:clj
   (comment
     (spit
      "/tmp/name-the-interval.org"
      (with-out-str
        (print
         (name-the-interval
          se.jherrlin.music-theory/interval-p
          se.jherrlin.music-theory/find-root-p
          se.jherrlin.music-theory/tones
          @se.jherrlin.music-theory/chords-atom))))))

(defn tone-in-interval [interval-f find-root-f all-tones chords-map]
  (let [;; interval-f  se.jherrlin.music-theory/interval-p
        ;; find-root-f se.jherrlin.music-theory/find-root-p
        ;; all-tones   se.jherrlin.music-theory/tones
        ;; chords-map  @se.jherrlin.music-theory/chords-atom
        ]
    (->> (for [{:keys [semitones] :as interval} (intervals)
               tone                             all-tones]
           (assoc interval :start-tone tone :end-tone (interval-f tone semitones)))
         (map (fn [{interval-name :name
                    :keys         [semitones function start-tone end-tone]}]
                (let [start-tone' (-> start-tone name str/upper-case)
                      end-tone'   (-> end-tone name str/upper-case)]
                  (str
                   "** " (fformat "%-60s:music:theory:intervals:drill:" (str "Tone interval " (str/lower-case interval-name) " from " start-tone'))
                   "\n\n"
                   "   " "What tone do you find a " interval-name " from " start-tone' "?"
                   "\n\n"
                   "*** Answer "
                   "\n\n    " end-tone'
                   "\n\n"))))
         (apply str))))

#?(:clj
   (comment
     (spit
      "/tmp/tone-in-interval.org"
      (with-out-str
        (print
         (tone-in-interval
          se.jherrlin.music-theory/interval-p
          se.jherrlin.music-theory/find-root-p
          se.jherrlin.music-theory/tones
          @se.jherrlin.music-theory/chords-atom))))))
