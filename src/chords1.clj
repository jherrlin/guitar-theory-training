(ns chords1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])
(count tones) ;; => 12

(def prim    #(nth % 0))
(def sekund  #(nth % 1))
(def ters    #(nth % 2))
(def kvart   #(nth % 3))
(def kvint   #(nth % 4))
(def sext    #(nth % 5))
(def septima #(nth % 6))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

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
   (let [tones'                 (find-root tone tones)
         major-scale-tones'     (major-scale-tones tones')
         minor-scale-tones'     (minor-scale-tones tones')
         minor-pentatonic-scale (->> minor-scale-tones'
                                     (vec-remove 1)
                                     (vec-remove 4))
         major-pentatonic-scale (->> major-scale-tones'
                                     (vec-remove 3)
                                     (vec-remove 5))]
     {:root                   tone
      :tones                  tones'
      :major-scale-chords     (chords-in-major-scale major-scale-tones')
      :major-scale-tones      major-scale-tones'
      :minor-scale-tones      minor-scale-tones'
      :minor-pentatonic-scale minor-pentatonic-scale
      :major-pentatonic-scale major-pentatonic-scale})))

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



;; ----------
;; Intervals
;; ----------
(defn unison           [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 0))
(defn major-second     [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 1))
(defn minor-second     [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 1) (flat tones)))
(defn major-third      [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 2))
(defn minor-third      [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 2) (flat tones)))
(defn perfect-fourth   [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 3))
;; tritones
(defn augmented-fourth [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 3) (sharp tones)))
(defn diminished-fifth [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 4) (flat tones)))
;;
(defn perfect-fifth    [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 4))
(defn major-sixth      [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 5))
(defn minor-sixth      [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 5) (flat tones)))
(defn major-seventh    [{:keys [major-scale-tones tones]}]     (nth major-scale-tones 6))
(defn minor-seventh    [{:keys [major-scale-tones tones]}] (-> (nth major-scale-tones 6) (flat tones)))
(def perfect-octave   unison)


(def interval-map
  {:unison           {:f       unison
                      :display "Unison"}
   :major-second     {:f       major-second
                      :display "Major second"}
   :minor-second     {:f       minor-second
                      :display "Minor second"}
   :major-third      {:f       major-third
                      :display "Major third"}
   :minor-third      {:f       minor-third
                      :display "Minor third"}
   :perfect-fourth   {:f       perfect-fourth
                      :display "Perfect fourth"}
   :augmented-fourth {:f       augmented-fourth
                      :display "Augmented fourth"}
   :diminished-fifth {:f       diminished-fifth
                      :display "Diminished fifth"}
   :perfect-fifth    {:f       perfect-fifth
                      :display "Perfect fifth"}
   :major-sixth      {:f       major-sixth
                      :display "Major sixth"}
   :minor-sixth      {:f       minor-sixth
                      :display "Minor sixth"}
   :major-seventh    {:f       major-seventh
                      :display "Major seventh"}
   :minor-seventh    {:f       minor-seventh
                      :display "Minor seventh"}
   :perfect-octave   {:f       perfect-octave
                      :display "Perfect octave"}})

(defn intervall-f [tone k]
  ((get-in interval-map [k :f]) (chord-base tone)))





;; How many semitones?
;; What note is in the minor second interval?

(=
 (-> :e chord-base augmented-fourth)
 (-> :e chord-base diminished-fifth))


(contains? (set (chord :e major-minor-seven)) :d)

(defn fret-table-with-tones [chord-tones]
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

(print
 (fret-table-with-tones (chord :e major)))

;; Org-drill
(print
 (str
  "#+STARTUP: overview\n\n"
  (str
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
                                    (str/replace "mmaj7" "m(maj7)"))
                     chord-desc (str (-> k name str/upper-case) " " (-> m name (str/replace "-" " ")))
                     tones-str  (->> tones (map (comp str/upper-case name)) (str/join ", "))]
                 (str
                  (str
                   "** " (format "%-60s:music:theory:chords:drill:" (str "Notes in " chord-name))
                   "\n\n"
                   "   What tones are in " chord-name " chord?"
                   "\n\n"
                   "   " chord-desc
                   "\n\n"
                   "*** Answer \n\n    " tones-str
                   "\n\n\n"
                   (fret-table-with-tones tones)
                   "\n")
                  (str
                   "** " (format "%-60s:music:theory:chords:drill:" (str "Name the chord: " tones-str))
                   "\n\n"
                   "   Name the chord with the following tones: " tones-str
                   "\n\n"
                   "*** Answer "
                   "\n\n    " chord-name
                   "\n    " chord-desc
                   "\n\n\n"
                   (fret-table-with-tones tones)
                   "\n")))))
        (apply str))
   (->> (for [tone  tones
              [k _] interval-map]
          (let [intervall'      k
                {:keys [tones]} (chord-base tone)
                intervall-tone  (intervall-f tone intervall')
                nr-of-semitones (->> tones
                                     (reverse)
                                     (drop-while #(not= % intervall-tone))
                                     count
                                     dec)]
            {:intervall       (get-in interval-map [k :display])
             :tone            (-> tone name str/upper-case)
             :nr-of-semitones nr-of-semitones
             :intervall-tone  (-> intervall-tone name str/upper-case)}))
        (map (fn [{:keys [intervall tone nr-of-semitones intervall-tone]}]
               (str
                (str
                 "** " (format "%-60s:music:theory:intervalls:drill:" (str intervall " from " tone))
                 "\n\n"
                 "   " "What is the tone in " intervall " intervall from tone " tone "?"
                 "\n"
                 "   " "How many semitones are in the intervall?"
                 "\n\n"
                 "*** Answer \n\n"
                 "    " "Tone in intervall:  " intervall-tone
                 "\n"
                 "    " "Numer of semitones: " nr-of-semitones
                 "\n\n")

                (str
                 "** " (format "%-60s:music:theory:intervalls:drill:" (str "Intervall " nr-of-semitones " semitones from " tone))
                 "\n\n"
                 "   " "Name the intervall " nr-of-semitones " semitones from " tone " and the tone in the intervall."
                 "\n\n"
                 "*** Answer \n\n"
                 "    " intervall
                 "\n\n"
                 "    " intervall-tone
                 "\n\n"))))
        (apply str))
   (->> (for [tone  tones
           [k s] [[:major-scale-tones "Major"]
                  [:minor-scale-tones "Minor"]
                  [:minor-pentatonic-scale "Minor pentatonic"]
                  [:major-pentatonic-scale "Major pentatonic"]]]
       (let [base'       (chord-base tone)
             scale-tones (get base' k)
             tones-str   (->> scale-tones (map (comp str/upper-case name)) (str/join ", "))]
         {:scale-tones scale-tones
          :display     s
          :tone        (-> tone name str/upper-case)
          :tones-str   tones-str}))
     (map (fn [{:keys [scale-tones display tone tones-str]}]
            (str
             "** " (format "%-60s:music:theory:scales:drill:" (str "Tones in " tone " " display " scale"))
             "\n\n"
             "   " "What tones are in the " tone " " display " scale?"
             "\n\n"
             "*** Answer \n\n"
             "    " "The tones are: " tones-str
             "\n\n\n"
             (fret-table-with-tones scale-tones)
             "\n")))
     (apply str)))))
