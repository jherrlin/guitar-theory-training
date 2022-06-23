(ns se.jherrlin.music-theory.utils
  (:require
   [clojure.string :as str]
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])))

#?(:cljs
   (defn format
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args)))

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

(defn fret-table-with-tones [tones chord-tones]
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

(defn find-chord-name [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (find-root root-tone all-tones)]
    (->> chords-map
         vals
         (filter (fn [{:keys [f] :as m}]
                   (= (set chord-tones) (set (f tones)))))
         (map (fn [{s :sufix
                    :as m}]
                (assoc m :chord-name (str (-> root-tone name str/upper-case)
                                          s))))
         (map :chord-name)
         first)))

(defn find-chord [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (find-root root-tone all-tones)]
    (->> (vals chords-map)
         (filter (fn [{:keys [f] :as m}]
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

(defn docstring->m [s]
  (when s
    (let [s             (str/trim s)
          [_ short]     (re-find #"short:\s*(\S*)\n" s)
          [_ full]      (re-find #"\n\s*full:\s*([a-zA-Z0-9() ]*)\n" s)
          [_ functions] (re-find #"\n\s*functions:\s*(.*)" s)]
      {:short     short
       :full      full
       :functions functions})))
