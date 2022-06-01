(ns chords)

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
(count tones) ;; => 12

(defn tone
  ([n]
   (tone n identity))
  ([n f]
   (fn [tones]
     (nth tones (mod (f n) 12)))))

(def prim    (partial tone 0))
(def sekund  (partial tone 1))
(def ters    (partial tone 2))
(def kvart   (partial tone 3))
(def kvint   (partial tone 4))
(def sext    (partial tone 5))
(def septima (partial tone 6))

(def flat dec)

((prim) tones)
;; => :c
((prim flat) tones)
;; => :b

(doseq [n (range 12)]
  (eval
   `(def ~(symbol (str "f" n)) (partial tone ~n))))

(defn find-root
  ""
  [root tones]
  {:pre [((set tones) root)]}
  (->> (cycle tones)
       (drop-while #(not= % root))
       (take 12)
       (vec)))

(defn succ [tones note]
  (let [idx (.indexOf tones note)]
    (nth tones (inc idx))))

(defn pred [tones note]
  (let [idx (.indexOf tones note)]
    (nth tones (dec idx))))

(defn chord [tones root major-or-minor & [color]]

  (let [f (apply comp fns)]
    (f (find-root root tones))))

(defn major )

(chord tones :c major)
(chord tones :c major seven)
(chord tones :c major maj-seven)
(chord tones :c minor seven)
(chord tones :c minor maj-seven)

(defn minor-steps [tones]
  [(nth tones 0)  ;; Hel
   (nth tones 2)  ;; Halv
   (nth tones 3)  ;; Hel
   (nth tones 5)  ;; Hel
   (nth tones 7)  ;; Halv
   (nth tones 8)  ;; Hel
   (nth tones 10) ;; Hel
   ])

(defn major-steps [tones]
  [(nth tones 0)  ;; Hel
   (nth tones 2)  ;; Hel
   (nth tones 4)  ;; Halv
   (nth tones 5)  ;; Hel
   (nth tones 7)  ;; Hel
   (nth tones 9)  ;; Hel
   (nth tones 10) ;; Halv
   ])

(find-root :c tones)

(def p partial)

(defmacro chord [])

(def major         (juxt (prim) (ters) (kvint)))
(def major-seventh (juxt (prim) (ters) (kvint) (septima flat)))

(major (find-root :c tones))





(find-root :e tones)
;; => (:e :f :f# :g :g# :a :a# :b :c :c# :d :d#)

(chord tones :e identity)
;; => (:e :f :f# :g :g# :a :a# :b :c :c# :d :d#)
(chord tones :e major)
;; => [:e :g# :b]
(chord tones :e minor)
;; => [:e :g :b]
