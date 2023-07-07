(ns v5.se.jherrlin.music-theory.definitions)


(def guitar                  [:e :a :d :g :b :e])
(def guitar-dadgad           [:d :a :d :g :a :d])
(def ukulele                 [:g :c :e :a])
(def bass                    [:e :a :d :g])
(def banjo                   [:c :g :d :a])
(def banjo-irish             [:g :d :a :e])
(def mandolin                [:g :d :a :e])
(def banjo-chicago           [:d :g :b :e])
(def mandola                 [:c :g :d :a])
(def octave-mandolin         [:g :d :a :e])


(def instrument-with-tuning
  {:guitar          {:tuning guitar
                     :text   "Standard guitar tuning"}
   :guitar-dadgad   {:tuning guitar-dadgad
                     :text   "Guitar in DADGAD tuning"}
   :bass            {:tuning bass
                     :text   "Standard bass tuning"}
   :ukulele         {:tuning ukulele
                     :text   "Standard ukulele tuning"}
   :banjo-standard  {:tuning banjo
                     :text   "Standard tenor tuning"}
   :banjo-irish     {:tuning banjo-irish
                     :text   "Irish tenor tuning"}
   :banjo-chicago   {:tuning banjo-chicago
                     :text   "Chicago tuning"}
   :mandolin        {:tuning mandolin
                     :text   "Mandolin standard tuning"}
   :mandola         {:tuning mandola
                     :text   "Mandola standard tuning"}
   :octave-mandolin {:tuning octave-mandolin
                     :text   "Octave mandolin standard tuning"}})

(defn tuning
  "Get tuning and text from key `k`"
  [k]
  (get instrument-with-tuning k))


;; ---------------
;; State / data
;; ---------------
(def chords (atom {}))
@chords

(def scales (atom {}))
@scales

(def chord-patterns (atom {}))
@chord-patterns

(def triad-patterns (atom {}))
@triad-patterns

(def mode-patterns (atom {}))
@mode-patterns

(def scale-patterns (atom {}))
@scale-patterns
;; ---------------
;; State / data end
;; ---------------

;; ---------------
;; Define helpers
;; ---------------
(defn define-chord
  ([chord-id intervals]
   (define-chord chord-id {} intervals))
  ([chord-id meta-data intervals]
   (let [chord (utils/define-chord chord-id meta-data intervals)
         id    (get chord :chord/id)]
     (swap! chords assoc id chord))))

(defn define-scale
  ([scale-id intervals-str]
   (define-scale scale-id {} intervals-str))
  ([scale-id meta-data intervals-str]
   (let [scale (utils/define-scale scale-id meta-data intervals-str)
         id    (get scale :scale/id)]
     (swap! scales assoc id scale))))

(defn define-chord-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :chord)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! chord-patterns assoc id chord-pattern))))

(defn define-triad-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :triad)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! triad-patterns assoc id chord-pattern))))

(defn define-mode-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :mode)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! mode-patterns assoc id chord-pattern))))

(defn define-scale-pattern
  ([pattern-name pattern]
   (define-chord-pattern pattern-name {} pattern))
  ([pattern-name meta-data pattern]
   (let [meta-data'    (assoc meta-data :type :scale)
         chord-pattern (utils/define-pattern pattern-name meta-data' pattern)
         id            (get chord-pattern :fretboard-pattern/id)]
     (swap! scale-patterns assoc id chord-pattern))))

;; ---------------
;; Define helpers end.
;; ---------------

;; ---------------
;; Chords
;; ---------------
(define-chord :major
  {:sufix        ""
   :display-text "major"
   :explanation  "major"}
  "1 3 5")
