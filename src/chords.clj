(ns chords)

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

(def prim    #(nth % 0))
(def sekund  #(nth % 1))
(def ters    #(nth % 2))
(def kvart   #(nth % 3))
(def kvint   #(nth % 4))
(def sext    #(nth % 5))
(def septima #(nth % 6))

(defn find-root
  [tone tones]
  {:pre [((set tones) tone)]}
  (->> (cycle tones)
       (drop-while #(not= % tone))
       (take 12)
       (vec)))

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

(def id identity)
(def sharp succ)
(def flat  pred)
(def half  1)
(def whole 2)
(def major-steps [whole whole half whole whole whole])
(def minor-steps #(assoc % 1 half 2 whole 4 half))
(defn intervall [xs]
  (->> xs
       (reduce
        (fn [xs' x]
          (conj xs' (+ (last xs') x))) [0])
       (map #(fn [xs] (nth xs %)))
       (apply juxt)))
(def major-scale-tones (->> major-steps intervall))
(def minor-scale-tones (->> major-steps minor-steps intervall))
(def triad (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def major (comp triad major-scale-tones))
(def minor (comp triad minor-scale-tones))
(def sus2  (comp (juxt #(nth % 0) #(nth % 1) #(nth % 4)) major-scale-tones))
(def sus4  (comp (juxt #(nth % 0) #(nth % 3) #(nth % 4)) major-scale-tones))

(-> major-steps minor-steps)

(major tones)
(minor tones)
(sus2  tones)
(sus4  tones)

(defn chord-p [tones root base & [color]]
  (let [color' (if color
                 (partial color tones (major-scale-tones tones))
                 identity)]
    (->> tones (find-root root) base color')))

(def chord (partial chord-p tones))

(defn seven [all-tones scale-tones triad]
  (conj triad (-> (septima scale-tones) (flat all-tones))))

(defn maj-seven [all-tones scale-tones triad]
  (conj triad (septima scale-tones)))


(chord :c major)            ;; => [:c :e :g]
(chord :c major seven)      ;; => [:c :e :g :a#]
(chord :c major maj-seven)  ;; => [:c :e :g :b]
(chord :c minor)            ;; => [:c :d# :g]
(chord :c minor seven)      ;; => [:c :d# :g :a#]
(chord :c minor maj-seven)  ;; => [:c :d# :g :b]
(chord :c sus2)             ;; => [:c :d :g]
(chord :c sus4)             ;; => [:c :f :g]

(chord :a major)  ;; => [:a :c# :e]
(chord :a minor)  ;; => [:a :c :e]

(chord :e major)  ;; => [:e :g# :b]
(chord :e minor)  ;; => [:e :g :b]
