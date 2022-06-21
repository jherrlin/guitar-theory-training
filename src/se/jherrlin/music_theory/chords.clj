(ns se.jherrlin.music-theory.chords
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils :as utils]
   [clojure.string :as str]))


(def chords
  "Contains chords data with intervals and meta data."
  (atom {}))

(comment
  @chords
  )

(defmacro defchord
  ([chord-name interval]
   `(defchord ~chord-name {} ~interval))
  ([chord-name meta-data interval]
   {:pre [(symbol? chord-name) (map? meta-data)]}
   (let [intervals'  (if (string? interval)
                      (->> interval
                           (re-seq #"b{0,2}#{0,2}\d")
                           (map #(get-in intervals/intervals-map-by-function [% :semitones])))
                      (seq interval))
         chord-name' (-> chord-name
                         str
                         (str/replace "-chord" ""))
         docstring   (if (string? interval)
                       interval
                       "")]
     (swap! chords assoc (keyword chord-name')
            (merge
             {:intervals intervals'
              :functions interval
              :name      chord-name'}
             meta-data))
     `(defn ~chord-name ~docstring [~'args]
        ((utils/juxt-intervals '~intervals') ~'args)))))

(comment
  (defchord major
    {:a :c}
    "1 3 5")

  (defchord minor
    "1 b3 5")

  (defchord minor-seven
    [0 3 7 10])


  @chords
  (macroexpand '(defchord major "hejsan" "1 3 5"))
  (major [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (minor [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (minor-seven [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  )
