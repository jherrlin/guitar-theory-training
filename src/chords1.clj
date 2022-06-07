(ns chords1
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

(def major (comp triad :major-scale-tones))

(def minor (comp triad :minor-scale-tones))

(def sus2  (comp (juxt #(nth % 0) #(nth % 1) #(nth % 4)) :major-scale-tones))

(def sus4  (comp (juxt #(nth % 0) #(nth % 3) #(nth % 4)) :major-scale-tones))

(defn major-seven [{:keys [tones root major-scale-tones minor-scale-tones]}]
  (conj (triad major-scale-tones) (-> (septima major-scale-tones) (flat tones))))

(def major-minor-seven major-seven)

(defn minor-seven [{:keys [tones root major-scale-tones minor-scale-tones]}]
  (conj (triad minor-scale-tones) (-> (septima major-scale-tones) (flat tones))))

(def minor-minor-seven minor-seven)

(defn minor-maj-seven [{:keys [tones root major-scale-tones minor-scale-tones]}]
  (conj (triad minor-scale-tones) (septima major-scale-tones)))

(defn major-maj-seven [{:keys [tones root major-scale-tones minor-scale-tones]}]
  (conj (triad major-scale-tones) (septima major-scale-tones)))

(defn minor-seven-flat-5 [{:keys [tones root major-scale-tones minor-scale-tones] :as m}]
  (let [[a b c d] (minor-seven m)]
    [a b (flat c tones) d]))

(defn major-seven-flat-5 [{:keys [tones root major-scale-tones minor-scale-tones] :as m}]
  (let [[a b c d] (major-seven m)]
    [a b (flat c tones) d]))

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
    [{:nr        1
      :intervall :prim
      :chord     (major-chord prim)
      :tones     prim}
     {:nr        2
      :intervall :sekund
      :chord     (minor-chord sekund)
      :tones     sekund}
     {:nr        3
      :intervall :ters
      :chord     (minor-chord ters)
      :tones     ters}
     {:nr        4
      :intervall :kvart
      :chord     (major-chord kvart)
      :tones     kvart}
     {:nr        5
      :intervall :kvint
      :chord     (major-chord kvint)
      :tones     kvint}
     {:nr        6
      :intervall :sext
      :chord     (minor-chord sext)
      :tones     sext}
     {:nr        7
      :intervall :septima
      :chord     (minor-chord septima)
      :tones     septima}]))

(defn chord-base
  ([tone]
   (chord-base tone tones))
  ([tone tones]
   (let [tones'             (find-root tone tones)
         major-scale-tones' (major-scale-tones tones')
         minor-scale-tones' (minor-scale-tones tones')]
     {:root               tone
      :tones              tones'
      :major-scale-chords (chords-in-major-scale major-scale-tones')
      :major-scale-tones  major-scale-tones'
      :minor-scale-tones  minor-scale-tones'})))

(chord-base :c)

(def chords-map
  {:major              major
   :minor              minor
   :sus2               sus2
   :sus4               sus4
   :major-seven        major-minor-seven
   :minor-seven        minor-minor-seven
   :minor-maj-seven    minor-maj-seven
   :major-maj-seven    major-maj-seven
   :minor-seven-flat-5 minor-seven-flat-5
   :major-seven-flat-5 major-seven-flat-5})

(defn chord [tone f]
  (f (chord-base tone)))

(->> (for [tone  tones
           [k f] chords-map]
       [tone k (chord tone f)])
     (map (fn [[k m tones]]
            (let [chord-name (-> (str (-> k name str/upper-case) " " (-> m name (str/replace "-" " ")))
                                 (str/replace "major" "")
                                 (str/replace "major" "")
                                 (str/replace "maj seven" "maj7")
                                 (str/replace "seven" "7")
                                 (str/replace "flat" "b")
                                 (str/replace "minor" "m")
                                 (str/replace #"\s+" "")
                                 (str/replace "mmaj7" "m(maj7)"))]
              (str
               "** " (format "%-40s:music:theory:chords:drill:" chord-name)
               "\n\n"
               "   What notes are in " chord-name " chord?"
               "\n\n"
               "   " (-> k name str/upper-case) " " (-> m name (str/replace "-" " "))
               "\n\n"
               "*** Answer \n\n   " (->> tones (map (comp str/upper-case name)) (str/join ", ")) "\n\n"))))
     (apply str)
     (print))
