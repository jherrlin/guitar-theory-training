(ns v5.se.jherrlin.music-theory.definitions
  (:require
   [v5.se.jherrlin.music-theory.utils :as utils]
   [v5.se.jherrlin.music-theory.models.chord :as models-chord]
   [v5.se.jherrlin.music-theory.models.scale :as models-scale]))


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
(comment
  ;; Chord model
  models-chord/Chord
  )
(defn define-chord
  "Define a chord and add it to the `chords` atom."
  ([id chord intervals]
   (define-chord id chord {} intervals))
  ([id chord meta-data intervals]
   {:pre [(uuid? id)]}
   (let [meta-data (assoc meta-data :chord chord)
         chord'    (utils/define-chord id meta-data intervals)
         chords    (swap! chords assoc id chord')]
     (if (models-chord/valid-chords? chords)
       chords
       (models-chord/explain-chords chords)))))


(comment
  ;; Scale model
  models-scale/Scale
  )
(defn define-scale
  ([id scale intervals-str]
   (define-scale id scale {} intervals-str))
  ([id scale meta-data intervals-str]
   {:pre [(uuid? id)]}
   (let [meta-data (assoc meta-data :scale scale)
         scale     (utils/define-scale id meta-data intervals-str)]
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

(random-uuid)

;; ---------------
;; Chords
;; ---------------
(define-chord
  #uuid "1cd72972-ca33-4962-871c-1551b7ea5244"
  :major
  {:sufix        ""
   :display-text "major"
   :explanation  "major"}
  "1 3 5")

(define-chord
  #uuid "bef8a634-b802-467a-bb35-f93643fbeb3d"
  :major-plus-5
  {:sufix        "+5"
   :explanation  "major where 5 is raised one half tone"}
  "1 3 #5")

(define-chord
  #uuid "f9426eb8-5046-474a-b4c9-62383e5b0345"
  :minor
  {:sufix        "m"
   :display-text "minor"
   :explanation  "minor"}
  "1 b3 5")

(define-chord
  #uuid "7dab3a40-8aff-4a10-9367-52a05d737f45"
  :major-maj-seven
  {:sufix       "maj7"
   :explanation "major maj 7th"}
  "1 3 5 7")

(define-chord
  #uuid "eebf1ac1-b3c5-46f1-87ac-f8d24823b730"
  :dominant-seven
  {:sufix       "7"
   :explanation "dominant 7th"}
  "1 3 5 b7")

(define-chord
  #uuid "5c957f40-8961-46a5-8e30-95fdfb827628"
  :dominant-seven-sharp-nine
  {:sufix       "7#9"
   :explanation "dominant 7th with a sharp nine"
   :text        "Also knows as the Hendrix chord."}
  "1 3 5 b7 #9")

(define-chord
  #uuid "e2f07542-bd79-424c-8bfc-401d12cb36d9"
  :dominant-seven-flat-nine
  {:sufix       "7b9"
   :explanation "dominant 7th"}
  "1 3 5 b7 b9")

(define-chord
  #uuid "5c217b88-f9d5-41bb-9b89-8589105d14dd"
  :minor-maj-seven
  {:sufix       "m(maj7)"
   :explanation "minor maj 7th"}
  "1 b3 5 7")

(define-chord
  #uuid "3e260b03-e6ce-485d-8b0c-9361a2566629"
  :minor-seven
  {:sufix       "m7"
   :explanation "minor 7th"}
  "1 b3 5 b7")

(define-chord
  #uuid "4f853d42-89bb-4bcb-9462-aeb4cd379fe4"
  :sus2
  {:sufix       "sus2"
   :explanation "suspended 2"}
  "1 2 5")

(define-chord
  #uuid "f2722ef1-f66f-468a-a428-fecd85a9200b"
  :augmented-triad
  {:sufix       "aug"
   :explanation "augmented triad"}
  "1 3 #5")

(define-chord
  #uuid "b490706e-a09d-485e-8d94-1f15c294eb5b"
  :sus4
  {:sufix       "sus4"
   :explanation "suspended 4"}
  "1 4 5")

(define-chord
  #uuid "1845518b-ace4-4baa-8083-6922ce33176f"
  :sus4-add-7
  {:sufix       "7sus4"
   :explanation "suspended 4 with added b7"}
  "1 4 5 b7")

(define-chord
  #uuid "b6855fb7-bdcf-4e18-a60e-95746bf8e7e9"
  :minor-seven-flat-5
  {:sufix       "m7b5"
   :text        "diminished half, often sufixed with Ø"
   :explanation "minor seven flat 5"}
  "1 b3 b5 b7")

(define-chord
  #uuid "07db914e-3a8b-4eeb-9024-5bc5a5c21a9c"
  :major-seven-flat-5
  {:sufix       "maj7b5"
   :explanation "major major seven flat 5"}
  "1 3 b5 7")

(define-chord
  #uuid "143fe631-8ce0-4c84-bcd8-60f39a354e78"
  :major-seven-sharp-5
  {:sufix             "(maj7)#5"
   :explanation       "major major seven sharp 5"
   :sufix-alternative "maj7#5"}
  "1 3 #5 7")

(define-chord
  #uuid "7d42b8e7-0b47-4b3c-af23-84461f12e723"
  :fifth
  {:sufix       "5"
   :explanation "5th"}
  "1 5")

(define-chord
  #uuid "1559e2cf-5f29-4831-8a8f-dddd7ad89580"
  :diminished-fifth
  {:sufix       "dim"
   :explanation "diminished fifth"}
  "1 b3 b5")

(define-chord
  #uuid "ede6dada-2c5d-4c63-af12-6569e89219c1"
  :diminished-seventh
  {:sufix       "dim7"
   :explanation "diminished seven"
   :text        "diminished whole, often sufixed with °"}
  "1 b3 b5 bb7")

(define-chord
  #uuid "c21c6b74-d211-4b05-a521-c8c2fc646c4c"
  :sixth
  {:sufix       "6"
   :explanation "sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 3 5 6")

(define-chord
  #uuid "8962c55d-9bb5-4f44-a810-a52f8251730a"
  :minor-sixth
  {:sufix       "m6"
   :explanation "minor sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 b3 5 6")

(define-chord
  #uuid "6cb2b165-bd51-4c36-a5f1-e64732897bbf"
  :ninth
  {:sufix       "9"
   :explanation "ninth"
   :text        "This is a dominant chord. The most important tones are 1, 3, b7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 b7 9")

(define-chord
  #uuid "1df9a391-b6a8-46b8-bb6e-5e2ed261022a"
  :maj-ninth
  {:sufix       "maj9"
   :explanation "major ninth"
   :text        "The most important tones are 1, 3, 7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 7 9")

(define-chord
  #uuid "f8ddfbbf-50d8-46af-a7e5-fc542cc82355"
  :minor-ninth
  {:sufix       "m9"
   :explanation "minor ninth # fifth is the least important tone, it may be ignored"
   :text        "The most important tones are 1, b3, b7 and 9. The 5 can be ignored in the chord."}
  "1 b3 5 b7 9")

(define-chord
  #uuid "065c90b0-a8dd-4137-9864-dc8b963c1f07"
  :minor-add9
  {:sufix       "m(add9)"
   :explanation "minor with an added 9"}
  "1 b3 5 9")

(define-chord
  #uuid "29942046-cae8-4647-bee4-969a090ee9b2"
  :minor-flat6
  {:sufix       "mb6"
   :explanation "minor with an added flat 6"}
  "1 b3 5 b6")

(define-chord
  #uuid "7c6a74ff-b643-419a-b14e-b4d666bf8115"
  :minor-sixth-added9
  {:sufix       "m6/9"
   :explanation "minor sixth with an added 9"}
  "1 b3 5 6 9")

(define-chord
  #uuid "aa156ab4-2ddb-49dc-bbdf-2f1f45cbd9e2"
  :maj-eleventh
  {:sufix       "maj11"
   :explanation "major eleventh"}
  "1 3 5 7 9 11")

(define-chord
  #uuid "8816ae91-e81f-4fec-930b-9597fd1c8efb"
  :eleventh
  {:sufix       "11"
   :explanation "dominant 11"}
  "1 3 5 b7 9 11")

(define-chord
  #uuid "2702633f-8919-4a41-84d5-84daf43c65db"
  :minor-eleventh
  {:sufix       "m11"
   :explanation "minor eleventh"}
  "1 b3 5 b7 9 11")

(define-chord
  #uuid "ecd08e90-1b69-4c26-8719-d82c1c101b28"
  :thirteen
  {:sufix       "13"
   :explanation "thirteenth. Dominant"}
  "1 3 5 b7 9 11 13")

(define-chord
  #uuid "2dfba16e-3816-461b-b088-ef2dcb394f43"
  :maj-thirteen
  {:sufix "13"
   :explanation "major thirteenth"}
  "1 3 5 7 9 11 13")

(define-chord
  #uuid "97465c6d-9c7a-4ee9-9ebd-336913569935"
  :minor-thirteen
  {:sufix "13"
   :explanation "major thirteenth"}
  "1 b3 5 7 9 11 13")
;; ---------------
;; Chords end
;; ---------------

;; ---------------
;; Scales
;; ---------------
(define-scale
  #uuid "39af7096-b5c6-45e9-b743-6791b217a3df"
  :major
  "1, 2, 3, 4, 5, 6, 7")

(define-scale
  #uuid "d091b747-63b9-4db2-9daa-6e9974852080"
  :minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale
  #uuid "8c0a7209-4ac4-4ec7-b8a5-e4fdaf449ad6"
  :lydian
  "1, 2, 3, #4, 5, 6, 7")

(define-scale
  #uuid "ee53b723-dc42-41ee-8467-61e5c0af3ffe"
  :ionian
  "1, 2, 3, 4, 5, 6, 7")

(define-scale
  #uuid "5a5a5751-9213-4116-9ca7-894cc00be2dc"
  :mixolydian
  "1, 2, 3, 4, 5, 6, b7")

(define-scale
  #uuid "825e9d0e-9b9d-461c-90b3-c2e71c9730aa"
  :dorian
  "1, 2, b3, 4, 5, 6, b7")

(define-scale
  #uuid "8136955a-a4cd-4978-98f6-4d4ff596b5db"
  :aeolian
  "1, 2, b3, 4, 5, b6, b7")

(define-scale
  #uuid "dbf44d25-75b9-4608-8e11-ba733b6edfc0"
  :phrygian
  "1, b2, b3, 4, 5, b6, b7")

(define-scale
  #uuid "862ef869-25e9-47f9-ab5e-8146f8a8b3e6"
  :locrian
  "1, b2, b3, 4, b5, b6, b7")

(define-scale
  #uuid "ddf306a1-b119-4eda-b3c3-1f5215cbe6d8"
  :harmonic-minor
  "1, 2, b3, 4, 5, b6, 7")

(define-scale
  #uuid "6d8e0cba-658d-4072-838e-3d50d926ed0f"
  :melodic-minor
  "1, 2, b3, 4, 5, 6, 7")

(define-scale
  #uuid "2b5857e2-e368-4847-ae91-d385e9286148"
  :natural-minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale
  #uuid "e7ad3188-1e4c-4d19-bd4b-99e97213c6f6"
  :pentatonic-major
  "1, 2, 3, 5, 6")

(define-scale
  #uuid "82751272-7c3a-445e-a589-24c1ad87a30e"
  :pentatonic-minor
  "1, b3, 4, 5, b7")

(define-scale
  #uuid "bac1ab62-34df-4232-b205-b197d25d8892"
  :pentatonic-blues
  "1, b3, 4, b5, 5, b7")

(define-scale
  #uuid "2dd35839-9d00-45bd-b4e8-43868aa9836c"
  :pentatonic-neutral
  "1, 2, 4, 5, b7")

(define-scale
  #uuid "0995cd25-a646-4347-918d-cfc9f06aa9a1"
  :diatonic
  "1, 2, 3, 5, 6")

(define-scale
  #uuid "c5500473-a4e1-44c3-833b-95d0c840c9e8"
  :diminished
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale
  #uuid "3df70e72-dd4c-4e91-85b5-13de2bb062ce"
  :mixolydian-blues-hybrid
  "1, 2, b3, 3, 4, b5, 5, 6, b7")

(define-scale
  #uuid "888bff34-1059-4f11-b5e0-79d6708dd3c7"
  :diminished-half
  "1, b2, b3, 3, b5, 5, 6, b7")

(define-scale
  #uuid "4bd060d3-6a6a-48d2-910e-4c2962af1f30"
  :diminished-whole
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale
  #uuid "5867c2d8-e2cf-4221-872e-dc2e50507abd"
  :diminished-whole-tone
  "1, b2, b3, 3, b5, b6, b7")

(define-scale
  #uuid "96b278ae-704e-46d7-9bd3-2b24bcd91d3b"
  :dominant-7th
  "1, 2, 3, 4, 5, 6, b7")

(define-scale
  #uuid "1694ffaf-7fcd-4447-ada4-42f272e6ae5e"
  :lydian-augmented
  "1, 2, 3, #4, #5, 6, 7")

(define-scale
  #uuid "6d32a072-cbbc-4b20-ae01-bda5a87071c6"
  :lydian-minor
  "1, 2, 3, #4, 5, b6, b7")

(define-scale
  #uuid "656b338a-7938-447b-ad89-c478023a21d4"
  :lydian-diminished
  "1, 2, b3, #4, 5, 6, 7")
;; ---------------
;; Scales end
;; ---------------
