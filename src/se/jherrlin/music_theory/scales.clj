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
   {:pre [(symbol? scale-name) (map? meta-data)]}
   (let [intervals'  (if (string? interval)
                       (->> interval
                            (re-seq #"b{0,2}#{0,2}\d")
                            (mapv #(get-in intervals/intervals-map-by-function [% :semitones])))
                       interval)
         scale-name' (-> scale-name
                         str
                         (str/replace "-scale" ""))
         docstring   (if (string? interval)
                       interval
                       "")
         f           (utils/juxt-intervals intervals')]
     (swap! scales assoc (keyword scale-name')
            (merge
             {:id        (keyword scale-name')
              :indexes   intervals'
              :functions interval
              :name      scale-name'
              :f         f
              :tags      (cond-> #{}
                           (contains? (set intervals') 3) (conj :minor)
                           (contains? (set intervals') 4) (conj :major)
                           (str/includes? interval "bb7") (conj :diminished)
                           (str/includes? interval "7")   (conj :seventh))}
             meta-data))
     `(defn ~scale-name ~docstring [~'args]
        ((utils/juxt-intervals ~intervals') ~'args)))))


(comment
  (defscale major
    {:a :c}
    "1 3 5")

  (defscale minor
    "1 b3 5")

  (defscale minor-seven
    [0 3 7 10])


  @scales
  (macroexpand '(defscale major "hejsan" "1 3 5"))
  ((get-in @scales [:major :f]) [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (major [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (minor [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  (minor-seven [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  )
