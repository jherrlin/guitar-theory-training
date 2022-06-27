(ns se.jherrlin.music-theory.utils
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])))

#?(:cljs
   (defn fformat
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args))
   :clj (def fformat format))

(defn find-root
  [tone tones]
  {:pre [((set tones) tone)]}
  (let [tones-count (count tones)]
    (->> (cycle tones)
         (drop-while #(not= % tone))
         (take tones-count)
         (vec))))

(defn juxt-intervals [intervals]
  (apply juxt
         (map
          (fn [interval] (fn [tones] (nth tones interval)))
          intervals)))

(defn fret-table-with-tones
  ([tones chord-tones]
   (fret-table-with-tones tones chord-tones 25))
  ([tones chord-tones nr-of-frets]
   (let [in-chord?   #(contains? (set chord-tones) %)
         show        #(if (in-chord? %)
                        (->> % name str/upper-case (fformat " %-3s"))
                        (fformat "%4s" ""))
         top-row (str "|" (str/join "|" (map #(fformat " %-3s" (str %)) (range 0 nr-of-frets))) "|")]
     (str
      top-row
      "\n"
      (str "|" (str/join "" (take (- (count top-row) 2) (repeat "-"))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :e tones) (cycle) (take nr-of-frets)))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :b tones) (cycle) (take nr-of-frets)))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :g tones) (cycle) (take nr-of-frets)))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :d tones) (cycle) (take nr-of-frets)))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :a tones) (cycle) (take nr-of-frets)))) "|")
      "\n"
      (str "|" (str/join "|" (map show (->> (find-root :e tones) (cycle) (take nr-of-frets)))) "|")
      "\n"))))

(defn find-chord-name [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (find-root root-tone all-tones)]
    (->> chords-map
         vals
         (filter (fn [{:chord/keys [f] :as m}]
                   (= (set chord-tones) (set (f tones)))))
         (map (fn [{s :chord/sufix}]
                (str (-> root-tone name str/upper-case) s)))
         first)))

(defn find-chord [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (find-root root-tone all-tones)]
    (->> chords-map
         (vals)
         (filter (fn [{:chord/keys [f] :as m}]
                   (= chord-tones (f tones))))
         (first))))

(defn pred [x xs]
  {:pre [((set xs) x)]}
  (->> (reverse xs)
       (cycle)
       (drop-while #(not= % x))
       (second)))

(defn succ [x xs]
  {:pre [((set xs) x)]}
  (->> xs
       (cycle)
       (drop-while #(not= % x))
       (second)))

(defn match-chord-with-scales [scales-map chord-indexes]
  (->> scales-map
       (vals)
       (filter (fn [scale]
                 (let [scale-indexes (get scale :scale/indexes)]
                   (set/subset? (set chord-indexes) (set scale-indexes)))))))

(defn define-chord
  ([intervals-map state name' intervals]
   (define-chord intervals-map state name' {} intervals))
  ([intervals-map state name' meta-data intervals]
   (let [indexes (->> intervals
                      (re-seq #"b{0,2}#{0,2}\d")
                      (mapv #(get-in intervals-map [% :semitones])))
         tags    (cond-> #{}
                   (contains? (set indexes) 3)     (conj :minor)
                   (contains? (set indexes) 4)     (conj :major)
                   (str/includes? intervals "bb7") (conj :diminished)
                   (str/includes? intervals "7")   (conj :seventh))]
     (swap! state assoc name'
            (assoc (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "chord/") keyword) v]))
                        (into {}))
                   :chord/id name'
                   :chord/intervals-xs (vec (re-seq #"b{0,2}#{0,2}\d" intervals))
                   :chord/intervals intervals
                   :chord/indexes indexes
                   :chord/title (-> name'
                              name
                              (str/replace "-" " "))
                   :chord/tags tags
                   :chord/f (juxt-intervals indexes))))))

(defn define-scale
  ([intervals-map state name' intervals]
   (define-scale intervals-map state name' {} intervals))
  ([intervals-map state name' meta-data intervals]
   (let [indexes (->> intervals
                      (re-seq #"b{0,2}#{0,2}\d")
                      (mapv #(get-in intervals-map [% :semitones])))
         tags    (cond-> #{}
                   (contains? (set indexes) 3) (conj :minor)
                   (contains? (set indexes) 4) (conj :major))]
     (swap! state assoc name'
            (assoc (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "scale/") keyword) v]))
                        (into {}))
                   :scale/id name'
                   :scale/intervals-xs (vec (re-seq #"b{0,2}#{0,2}\d" intervals))
                   :scale/intervals intervals
                   :scale/indexes indexes
                   :scale/title (-> name'
                              name
                              (str/replace "-" " "))
                   :scale/tags tags
                   :scale/f (juxt-intervals indexes))))))
