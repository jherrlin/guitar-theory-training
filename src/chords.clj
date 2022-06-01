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

(def sharp succ)
(def flat  pred)

(let [t (fn [x] (fn [xs] (nth xs x)))]
  ;;                   Hel   Hel   Halv  Hel   Hel   Hel
  (def major (juxt (t 0) (t 2) (t 4) (t 5) (t 7) (t 9) (t 11)))

  ;;                   Hel   Halv  Hel   Hel   Halv  Hel
  (def minor (juxt (t 0) (t 2) (t 3) (t 5) (t 7) (t 8) (t 10)))

  (def triad (juxt (t 0) (t 2) (t 4)))
  )

(defn chord-p [tones root major-or-minor & [color]]
  (let [color' (if color
                 (partial color tones (major tones))
                 identity)]
    (->> tones (find-root root) major-or-minor triad color')))

(def chord (partial chord-p tones))

(defn seven [all-tones scale-tones triad]
  (conj triad (-> (septima scale-tones) (flat all-tones))))

(defn maj-seven [all-tones scale-tones triad]
  (conj triad (septima scale-tones)))


(minor tones) ;; => [:c :d :d# :f :g :g# :a#]
(major tones) ;; => [:c :d :e  :f :g :a  :b]

(chord :c major)            ;; => [:c :e :g]
(chord :c major seven)      ;; => [:c :e :g :a#]
(chord :c major maj-seven)  ;; => [:c :e :g :b]
(chord :c minor)            ;; => [:c :d# :g]
(chord :c minor seven)      ;; => [:c :d# :g :a#]
(chord :c minor maj-seven)  ;; => [:c :d# :g :b]

(chord :a major)  ;; => [:a :c# :e]
(chord :a minor)  ;; => [:a :c :e]

(chord :e major)  ;; => [:e :g# :b]
(chord :e minor)  ;; => [:e :g :b]
