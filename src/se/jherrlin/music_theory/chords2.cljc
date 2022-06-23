(ns se.jherrlin.music-theory.chords2
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils :as utils]
   [clojure.string :as str]))


(def chords-map
  {:major {:id          :major
           :sufix       ""
           :explanation "major"
           :intervals   "1 3 5"}
   :minor {:id          :minor
           :sufix       "m"
           :explanation "minor"
           :intervals   "1 b3 5"}
   :sus2  {:id          :sus2
           :sufix       "sus2"
           :explanation "suspended 2"
           :intervals   "1 2 5"}
   :sus4  {:id          :sus4
           :sufix       "sus4"
           :explanation "suspended 4"
           :intervals   "1 4 5"}})

(defn index [intervals-map intervals]
  (->> intervals
       (re-seq #"b{0,2}#{0,2}\d")
       (mapv #(get-in intervals-map [% :semitones]))))

(def index-p (partial index intervals/intervals-map-by-function))

(defn indexes [intervals-map chords]
  (->> chords
       (map (fn [{:keys [intervals] :as m}]
              (assoc m :indexes (index intervals-map intervals))))))

(def indexes-p (partial indexes intervals/intervals-map-by-function))

(comment
  (index intervals/intervals-map-by-function "1 3 5")
  (index-p "1 3 5")

  (indexes-p chords-map)
  )

(defn tag [{:keys [intervals indexes] :as chord}]
  (cond-> #{}
    (contains? (set indexes) 3) (conj :minor)
    (contains? (set indexes) 4) (conj :major)
    (str/includes? intervals "bb7") (conj :diminished)
    (str/includes? intervals "7")   (conj :seventh)))

(defn tags [chords]
  (->> chords
       (map (fn [chord]
              (assoc chord :tags (tag chord))))))

(defn fns [chords]
  (->> chords
       (map (fn [{:keys [indexes] :as chord}]
              (assoc chord :f (utils/juxt-intervals indexes))))))

(def chords
  (->> chords-map
       vals
       indexes-p
       tags
       fns
       (map (juxt :id identity))
       (into {})))

(defn chord [chords-map what-chord tones]
  {:pre [(keyword? what-chord)]}
  ((get-in chords-map [what-chord :f]) tones))

(def chord-p (partial chord chords))

(comment
  chords
  (chord-p :major [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
  )
