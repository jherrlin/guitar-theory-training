(ns chords
  (:require [clojure.string :as str]))

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

(defn chord
  ([root base]
   (chord tones root base nil))
  ([root base color]
   (chord tones root base color))
  ([tones root base & [color]]
   (let [color' (if color
                  (partial color tones (major-scale-tones tones))
                  identity)]
     (->> tones (find-root root) base color'))))

(defn seven [all-tones scale-tones triad]
  (conj triad (-> (septima scale-tones) (flat all-tones))))

(defn maj-seven [all-tones scale-tones triad]
  (conj triad (->> all-tones)))


(->> (find-root :c tones)
     major-scale-tones)


(defn chords-in-major-scale
  "Chords in scale.
  In:  `[:c :d :e :f :g :a :b]`
  Out: `[,,, {:intervall :prim, :chord :c, :tones [:c :e :g]} ,,,]`"
  [scale-tones]
  (let [[prim sekund ters kvart kvint sext septima]
        (map vector
             scale-tones
             (take 7 (drop 2 (cycle scale-tones)))
             (take 7 (drop 4 (cycle scale-tones))))
        major-chord #(-> % first)
        minor-chord #(-> % first name (str "m") keyword)]
    [{:intervall :prim
      :chord     (major-chord prim)
      :tones     prim}
     {:intervall :sekund
      :chord     (minor-chord sekund)
      :tones     sekund}
     {:intervall :ters
      :chord     (minor-chord ters)
      :tones     ters}
     {:intervall :kvart
      :chord     (major-chord kvart)
      :tones     kvart}
     {:intervall :kvint
      :chord     (major-chord kvint)
      :tones     kvint}
     {:intervall :sext
      :chord     (minor-chord sext)
      :tones     sext}
     {:intervall :septima
      :chord     (minor-chord septima)
      :tones     septima}]))

(-> (find-root :e tones)
    major-scale-tones
    chords-in-major-scale)


(->> (for [tone  tones
           [k f] {:major           #(chord % major)
                  :major-seven     #(chord % major seven)
                  :major-maj-seven #(chord % major maj-seven)
                  :minor           #(chord % minor)
                  :minor-seven     #(chord % minor seven)
                  :minor-maj-seven #(chord % minor maj-seven)
                  :sus2            #(chord % sus2)
                  :sus4            #(chord % sus4)}]
       [tone k (f tone)])
     (map (fn [[k m tones]]
            (str
             "** " (-> k name str/upper-case) " " (-> m name (str/replace "-" " ")) "\t\t\t\t:music:theory:chords:drill:"
             "\n\n"
             "   What notes are in the " (-> k name str/upper-case) " " (-> m name (str/replace "-" " ")) " chord?"
             "\n\n"
             "*** Answer \n\n   " tones "\n\n")))
     (apply str)
     (print))


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



(find-root :g# tones)                         ;; => [:g# :a :a# :b :c :c# :d :d# :e :f :f# :g]
                                              ;;     1   2   3  4   5   6  7
(-> (find-root :g# tones) major-scale-tones)  ;; => [:g# :a# :c :c# :d# :f :g]
(-> (find-root :g# tones) minor-scale-tones)  ;; => [:g# :a# :b :c# :d# :e :f#]



(chord :g# minor maj-seven) ;; => [:g# :b :d# :b]
































































(defn chord-base
  ([tone]
   (chord-base tone tones))
  ([tone tones]
   (let [tones'             (find-root tone tones)
         major-scale-tones' (major-scale-tones tones')
         minor-scale-tones' (minor-scale-tones tones')]
     {:root              tone
      :tones             tones'
      :major-scale-tones major-scale-tones'
      :minor-scale-tones minor-scale-tones'})))

(def major)

(chord-base :g#)
