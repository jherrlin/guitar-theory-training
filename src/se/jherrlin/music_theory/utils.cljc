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

(defn tone->str
  [x]
  (let [x (if (keyword? x)
            (name x)
            x)]
    (-> x str/lower-case str/capitalize)))

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
                        (->> % name tone->str (fformat " %-3s"))
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
         (filter (fn [{:chord/keys [f]}]
                   (= (set chord-tones) (set (f tones)))))
         (map (fn [{s :chord/sufix}]
                (str (-> root-tone tone->str) s)))
         first)))

(defn find-chord [chords-map all-tones chord-tones]
  (let [[root-tone & _] chord-tones
        tones           (find-root root-tone all-tones)]
    (->> chords-map
         (vals)
         (filter (fn [{:chord/keys [f]}]
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

(defn inteval->matrix [interval]
  (->> interval
       (str/trim)
       (str/split-lines)
       (map str/trim)
       (mapv (fn [line]
               (->> line
                    (re-seq #"(b{0,2}#{0,2}\d)|-")
                    (mapv (comp #(when-not (= "-" %) %) first)))))))

(defn define-chord-pattern
  ([chord-patterns-atom intervals-map-by-function pattern-name pattern]
   (define-chord-pattern chord-patterns-atom intervals-map-by-function pattern-name {} pattern))
  ([chord-patterns-atom intervals-map-by-function pattern-name meta-data pattern]
   (let [pattern'
         (->> pattern
              (str/trim)
              (str/split-lines)
              (map str/trim)
              (mapv #(->> %
                          (re-seq #"(b{0,2}#{0,2}\d)|-")
                          (mapv (comp
                                 (fn [s]
                                   (when-not (= s "-")
                                     (get-in intervals-map-by-function [s :semitones])))
                                 first)))))
         meta-data (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "chord-pattern/") keyword) v]))
                        (into {}))]
     (swap! chord-patterns-atom assoc pattern-name
            (assoc meta-data
                   :chord/pattern-matrix (inteval->matrix pattern)
                   :chord/pattern pattern'
                   :chord/pattern-id pattern-name
                   :chord/pattern-title (name pattern-name)
                   :chord/pattern-str pattern)))))

(defn define-mode
  ([modes-atom intervals-map-by-function pattern-name pattern]
   (define-mode modes-atom intervals-map-by-function pattern-name {} pattern))
  ([modes-atom intervals-map-by-function pattern-name meta-data pattern]
   (let [pattern'
         (->> pattern
              (str/trim)
              (str/split-lines)
              (map str/trim)
              (mapv #(->> %
                          (re-seq #"(b{0,2}#{0,2}\d)|-")
                          (mapv (comp
                                 (fn [s]
                                   (when-not (= s "-")
                                     (get-in intervals-map-by-function [s :semitones])))
                                 first)))))
         meta-data (->> meta-data
                        (map (fn [[k v]]
                               [(->> k name (str "mode/") keyword) v]))
                        (into {}))]
     (swap! modes-atom assoc pattern-name
            (assoc meta-data
                   :mode/pattern-matrix (inteval->matrix pattern)
                   :mode/pattern pattern'
                   :mode/id pattern-name
                   :mode/title (name pattern-name)
                   :mode/pattern-str pattern)))))

(def triad   (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def seventh (juxt #(nth % 0) #(nth % 2) #(nth % 4) #(nth % 6)))

(defn diatonic-chord-progressions [triad-or-seven-map scales-map chords-map all-tones tone kind triad-or-seven]
  {:pre [(keyword? tone) (keyword? kind)]}
  (let [scale-tones ((get-in scales-map [kind :scale/f]) (find-root tone all-tones))]
    (->> scale-tones
         (reduce
          (fn [m t]
            (let [chord-tones ((get triad-or-seven-map triad-or-seven) (find-root t scale-tones))
                  chord-name  (find-chord-name chords-map all-tones chord-tones)]
              (conj m {:harmonization/key-of tone
                       :harmonization/kind   kind
                       :chord/name           chord-name
                       :chord/tones          chord-tones})))
          [])
         (mapv
          #(assoc %7
                  :harmonization/index %1 :harmonization/position %2 :harmonization/mode %3 :harmonization/mode-str %4 :harmonization/family %5 :harmonization/family-str %6)
          (range 1 100)
          (if (= kind :major)
            ["I" "ii" "iii" "IV" "V" "vi" "vii"]
            ["i" "ii" "III" "iv" "v" "VI" "VII"])
          (if (= kind :major)
            [:ionian  :dorian  :phrygian :lydian :mixolydian :aeolian :locrian]
            [:aeolian :locrian :ionian   :dorian :phrygian   :lydian  :mixolydian])
          (if (= kind :major)
            ["Ionian"  "Dorian"  "Phrygian" "Lydian" "Mixolydian" "Aeolian" "Locrian"]
            ["Aeolian" "Locrian" "Ionian"   "Dorian" "Phrygian"   "Lydian"  "Mixolydian"])
          (if (= kind :major)
            [:tonic :subdominant :tonic :subdominant :dominant :tonic :dominant]
            [:tonic :subdominant :tonic :subdominant :dominant :subdominant :dominant])
          (if (= kind :major)
            ["T" "S" "T" "S" "D" "T" "D"]
            ["T" "S" "T" "S" "D" "S" "D"]))
         (mapv (fn [{:chord/keys [tones] :as m}]
                 (merge m (find-chord chords-map all-tones tones))))
         (mapv (fn [{:chord/keys [indexes] :as m}]
                 (assoc m :matching-scales (match-chord-with-scales scales-map indexes)))))))

(defn diatonic-chord-progressions-str [xs]
  (str
   "  T = Tonic (stable), S = Subdominant (leaving), D = Dominant (back home)"
   "\n\n"
   (->> xs (map (comp #(fformat "  %-10s" %) str :harmonization/index)) (str/join) (str/trim))
   "\n"
   (->> xs (map (comp #(fformat "  %-10s" %) str :harmonization/position)) (str/join) (str/trim))
   "\n"
   (->> xs (map (comp #(fformat "  %-10s" %) str :harmonization/mode-str)) (str/join) (str/trim))
   "\n"
   (->> xs (map (comp #(fformat "  %-10s" %) str :harmonization/family-str)) (str/join) (str/trim))
   "\n"
   (->> xs (map (comp #(fformat "  %-10s" %) str :chord/name)) (str/join) (str/trim))))

(defn list-insert [lst elem index]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))

(defn locate-pattern-on-fret
  [find-root-f interval-f string-tunings root-tone mode-spec]
  (let [mode-pred-lenght (-> mode-spec first count)
        fret-tones'      (->> string-tunings
                              (mapv #(->> (find-root-f %)
                                          (cycle)
                                          (take 25)
                                          (vec))))]
    (loop [counter 0]
      (let [combinations
            (->>  fret-tones'
                  (mapv (comp vec (partial take mode-pred-lenght) (partial drop counter)))
                  (apply concat)
                  (mapv vector (apply concat mode-spec)))
            box-match? (->> combinations
                            (remove (comp nil? first))
                            (every? (fn [[interval' tone']]
                                      (= (interval-f root-tone interval') tone'))))]
        (if box-match?
          {:root-starts-at-fret counter
           :fret                (->> combinations
                                     (mapv (fn [[interval' tone']]
                                             (when (and interval' (= (interval-f root-tone interval') tone'))
                                               {:interval interval'
                                                :tone     tone'})))
                                     (partition mode-pred-lenght))}
          (recur (inc counter)))))))

(defn padding-fret-pattern [{:keys [root-starts-at-fret fret]}]
  (->> fret
       (mapv (fn [row]
               (let [row-with-prefix        (concat (take root-starts-at-fret (repeat nil)) row)
                     row-with-prefix-length (count row-with-prefix)]
                 (vec (concat row-with-prefix (take (- 16 row-with-prefix-length) (repeat nil)))))))))

(defn fret-pattern-to-str [fret]
  (let [fret (reverse fret)
        get-string (fn [n] (map (comp #(if (nil? %) "" (-> % tone->str)) :tone) (nth fret n)))
        rows       (->> [(map str (range 16))
                         (get-string 5)
                         (get-string 4)
                         (get-string 3)
                         (get-string 2)
                         (get-string 1)
                         (get-string 0)]
                        (map (fn [row]
                               (apply str (interpose "|" (map #(fformat " %-3s" %) row)))))
                        (map (fn [row]
                               (str "|" row "|"))))
        row-length (-> rows first count)]
    (->> (list-insert rows (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
         (str/join "\n"))))

(defn index-of [x xs]
  (loop [counter       0
         [this & rest] xs]
    (cond
      (= this x)
      counter
      (nil? rest)
      nil
      :else
      (recur (inc counter) rest))))

(defn interval->tone
  [intervals-map-by-function get-in-intervals-map-f
   find-root-f sharp-tones flat-tones tone interval]
  {:pre [(keyword? tone) (string? interval)]}
  (let [tones      (if (str/includes? interval "b")
                     flat-tones sharp-tones)
        flat-tone? (-> tone name (str/includes? "b"))
        tone-idx   (if flat-tone?
                     (index-of tone flat-tones)
                     (index-of tone sharp-tones))]
    (nth
     (find-root-f (nth tones tone-idx) tones)
     (get-in-intervals-map-f intervals-map-by-function))))

(defn fretboard-string [flat-tones sharp-tones string-tune]
  {:pre [(contains? (set (concat flat-tones sharp-tones)) string-tune)]}
  (let [index (index-of
               string-tune
               (concat sharp-tones flat-tones))]
    (->> (mapv
          (fn [x s f]
            (let [tone (set [s f])]
              {:x          x
               :tone       tone
               :flat-tone  f
               :sharp-tone s
               :tone-pred  (fn [t] (boolean (tone t)))}))
          (iterate inc 0)
          (find-root (nth sharp-tones (mod index 12)) sharp-tones)
          (find-root (nth flat-tones (mod index 12)) flat-tones)))))

(defn fretboard-strings
  [flat-tones sharp-tones string-tunes]
  (->> string-tunes
       (mapv
        (fn [y string-tune]
          (mapv
           #(assoc % :y y)
           (fretboard-string flat-tones sharp-tones string-tune)))
        (iterate inc 0))))

(defn intervals-and-key-to-fretboard-matrix
  [fretboard-strings-f interval->tone-f tuning key-of intervals fretboard-length]
  (let [fretboard (fretboard-strings-f tuning)]
    (->> fretboard
         (map
          (fn [row]
            (->> row
                 (map
                  (fn [{:keys [flat-tone sharp-tone]}]
                    (->> intervals
                         (map (partial interval->tone-f key-of))
                         (map (fn [t]
                                (condp = t
                                  sharp-tone sharp-tone
                                  flat-tone  flat-tone
                                  nil)))
                         (remove nil?)
                         (first))))
                 (cycle)
                 (take fretboard-length)))))))

(defn intervals-and-key-to-fretboard-matrix-str
  [matrix]
  (let [matrix-with-nrs
        (concat
         [(-> matrix first count range)]
         matrix)
        rows
        (->> matrix-with-nrs
             (map
              (fn [row]
                (->> row
                     (map
                      #(cond
                         (nil? %)    ""
                         (number? %) (str %)
                         :else
                         (tone->str %))))))
             (map (fn [row]
                    (apply str (interpose "|" (map #(fformat " %-3s" %) row)))))
             (map (fn [row]
                    (str "|" row "|"))))
        row-length      (-> rows first count)]
    (->> (list-insert rows (str "|" (apply str (take (- row-length 2) (repeat "-"))) "|") 1)
         (str/join "\n"))))
