(ns utils
  (:require [clojure.string :as str]))

(defn find-root
  [tone tones]
  {:pre [((set tones) tone)]}
  (->> (cycle tones)
       (drop-while #(not= % tone))
       (take 12)
       (vec)))

(defn juxt-intervals [intervals]
  (apply juxt
         (map
          (fn [interval] (fn [tones] (nth tones interval)))
          intervals)))

(defn fret-table-with-tones [chord-tones tones]
  (let [in-chord?   #(contains? (set chord-tones) %)
        show        #(if (in-chord? %)
                       (->> % name str/upper-case (format " %-3s"))
                       (format "%4s" ""))]
    (str
     (str "|" (str/join "|" (map #(format " %-3s" %) (range 0 25))) "|")
     "\n"
     (str "|" (str/join "" (take 124 (repeat "-"))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :e tones) (cycle) (take 25)))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :b tones) (cycle) (take 25)))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :g tones) (cycle) (take 25)))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :d tones) (cycle) (take 25)))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :a tones) (cycle) (take 25)))) "|")
     "\n"
     (str "|" (str/join "|" (map show (->> (find-root :e tones) (cycle) (take 25)))) "|")
     "\n")))
