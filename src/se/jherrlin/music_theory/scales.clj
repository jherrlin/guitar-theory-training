(ns se.jherrlin.music-theory.scales
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils :as utils]
   [clojure.string :as str]))


(def scales
  "Contains scales data with intervals and meta data."
  (atom {}))

(comment
  @scales
  )

(defmacro defscale
  ([scale-name interval]
   `(defscale ~scale-name {} ~interval))
  ([scale-name meta-data interval]
   {:pre [(symbol? scale-name) (map? meta-data) (string? interval)]}
   (let [intervals'
         (->> interval
              (re-seq #"b{0,2}#{0,2}\d")
              (map #(get-in intervals/intervals-map-by-function [% :semitones])))
         scale-name' (-> scale-name
                         str
                         (str/replace "-scale" ""))]
     (swap! scales assoc (keyword scale-name')
            (merge
             {:intervals intervals'
              :functions interval
              :name      scale-name'}
             meta-data))
     `(defn ~scale-name ~interval [~'args]
        ((utils/juxt-intervals '~intervals') ~'args)))))

(comment
  (defscale major
    {:a :c}
    "1 3 5")

  (defscale minor
    "1 b3 5")

  @scales
  (major [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (minor [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  )
