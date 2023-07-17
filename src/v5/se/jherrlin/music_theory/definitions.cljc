(ns v5.se.jherrlin.music-theory.definitions
  (:require
   [v5.se.jherrlin.music-theory.utils :as utils]
   [v5.se.jherrlin.music-theory.models.chord :as models-chord]
   [v5.se.jherrlin.music-theory.models.scale :as models-scale]
   [v5.se.jherrlin.music-theory.models.fretboard-pattern :as models-fretboard-pattern]))


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

(comment
  ;; fretboard patterns model
  models-fretboard-pattern/FretboardPattern
  )

(defn define-chord-pattern
  [id meta-data pattern]
  (let [meta-data'    (assoc meta-data :type :chord)
        chord-pattern (utils/define-pattern id meta-data' pattern)]
    (if (models-fretboard-pattern/validate-fretboard-pattern? chord-pattern)
      (swap! chord-patterns assoc id chord-pattern)
      (throw
       (ex-info
        "Chord pattern error"
        (models-fretboard-pattern/explain-fretboard-pattern chord-pattern))))))

(defn define-triad-pattern
  [id meta-data pattern]
  (let [meta-data'    (assoc meta-data :type :triad)
        chord-pattern (utils/define-pattern id meta-data' pattern)]
    (if (models-fretboard-pattern/validate-fretboard-pattern? chord-pattern)
      (swap! triad-patterns assoc id chord-pattern)
      (throw
       (ex-info
        "Triad pattern error"
        (models-fretboard-pattern/explain-fretboard-pattern chord-pattern))))))

(defn define-mode-pattern
  [id meta-data pattern]
  (let [meta-data'    (assoc meta-data :type :mode)
        chord-pattern (utils/define-pattern id meta-data' pattern)]
    (if (models-fretboard-pattern/validate-fretboard-pattern? chord-pattern)
      (swap! mode-patterns assoc id chord-pattern)
      (throw
       (ex-info
        "Mode pattern error"
        (models-fretboard-pattern/explain-fretboard-pattern chord-pattern))))))

(defn define-scale-pattern
  [id meta-data pattern]
  (let [meta-data'    (assoc meta-data :type :scale)
        chord-pattern (utils/define-pattern id meta-data' pattern)]
    (if (models-fretboard-pattern/validate-fretboard-pattern? chord-pattern)
      (swap! scale-patterns assoc id chord-pattern)
      (throw
       (ex-info
        "Scale pattern error"
        (models-fretboard-pattern/explain-fretboard-pattern chord-pattern))))))
;; ---------------
;; Define helpers end.
;; ---------------

;; ---------------
;; Chords
;; ---------------
(define-chord #uuid "1cd72972-ca33-4962-871c-1551b7ea5244"
  :major
  {:sufix        ""
   :display-text "major"
   :explanation  "major"}
  "1 3 5")

(define-chord #uuid "bef8a634-b802-467a-bb35-f93643fbeb3d"
  :major-plus-5
  {:sufix        "+5"
   :explanation  "major where 5 is raised one half tone"}
  "1 3 #5")

(define-chord #uuid "f9426eb8-5046-474a-b4c9-62383e5b0345"
  :minor
  {:sufix        "m"
   :display-text "minor"
   :explanation  "minor"}
  "1 b3 5")

(define-chord #uuid "7dab3a40-8aff-4a10-9367-52a05d737f45"
  :major-maj-seven
  {:sufix       "maj7"
   :explanation "major maj 7th"}
  "1 3 5 7")

(define-chord #uuid "eebf1ac1-b3c5-46f1-87ac-f8d24823b730"
  :dominant-seven
  {:sufix       "7"
   :explanation "dominant 7th"}
  "1 3 5 b7")

(define-chord #uuid "5c957f40-8961-46a5-8e30-95fdfb827628"
  :dominant-seven-sharp-nine
  {:sufix       "7#9"
   :explanation "dominant 7th with a sharp nine"
   :text        "Also knows as the Hendrix chord."}
  "1 3 5 b7 #9")

(define-chord #uuid "e2f07542-bd79-424c-8bfc-401d12cb36d9"
  :dominant-seven-flat-nine
  {:sufix       "7b9"
   :explanation "dominant 7th"}
  "1 3 5 b7 b9")

(define-chord #uuid "5c217b88-f9d5-41bb-9b89-8589105d14dd"
  :minor-maj-seven
  {:sufix       "m(maj7)"
   :explanation "minor maj 7th"}
  "1 b3 5 7")

(define-chord #uuid "3e260b03-e6ce-485d-8b0c-9361a2566629"
  :minor-seven
  {:sufix       "m7"
   :explanation "minor 7th"}
  "1 b3 5 b7")

(define-chord #uuid "4f853d42-89bb-4bcb-9462-aeb4cd379fe4"
  :sus2
  {:sufix       "sus2"
   :explanation "suspended 2"}
  "1 2 5")

(define-chord #uuid "f2722ef1-f66f-468a-a428-fecd85a9200b"
  :augmented-triad
  {:sufix       "aug"
   :explanation "augmented triad"}
  "1 3 #5")

(define-chord #uuid "b490706e-a09d-485e-8d94-1f15c294eb5b"
  :sus4
  {:sufix       "sus4"
   :explanation "suspended 4"}
  "1 4 5")

(define-chord #uuid "1845518b-ace4-4baa-8083-6922ce33176f"
  :sus4-add-7
  {:sufix       "7sus4"
   :explanation "suspended 4 with added b7"}
  "1 4 5 b7")

(define-chord #uuid "b6855fb7-bdcf-4e18-a60e-95746bf8e7e9"
  :minor-seven-flat-5
  {:sufix       "m7b5"
   :text        "diminished half, often sufixed with Ø"
   :explanation "minor seven flat 5"}
  "1 b3 b5 b7")

(define-chord #uuid "07db914e-3a8b-4eeb-9024-5bc5a5c21a9c"
  :major-seven-flat-5
  {:sufix       "maj7b5"
   :explanation "major major seven flat 5"}
  "1 3 b5 7")

(define-chord #uuid "143fe631-8ce0-4c84-bcd8-60f39a354e78"
  :major-seven-sharp-5
  {:sufix             "(maj7)#5"
   :explanation       "major major seven sharp 5"
   :sufix-alternative "maj7#5"}
  "1 3 #5 7")

(define-chord #uuid "7d42b8e7-0b47-4b3c-af23-84461f12e723"
  :fifth
  {:sufix       "5"
   :explanation "5th"}
  "1 5")

(define-chord #uuid "1559e2cf-5f29-4831-8a8f-dddd7ad89580"
  :diminished-fifth
  {:sufix       "dim"
   :explanation "diminished fifth"}
  "1 b3 b5")

(define-chord #uuid "ede6dada-2c5d-4c63-af12-6569e89219c1"
  :diminished-seventh
  {:sufix       "dim7"
   :explanation "diminished seven"
   :text        "diminished whole, often sufixed with °"}
  "1 b3 b5 bb7")

(define-chord #uuid "c21c6b74-d211-4b05-a521-c8c2fc646c4c"
  :sixth
  {:sufix       "6"
   :explanation "sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 3 5 6")

(define-chord #uuid "8962c55d-9bb5-4f44-a810-a52f8251730a"
  :minor-sixth
  {:sufix       "m6"
   :explanation "minor sixth"
   :text        "There are 4 tones in sixth chords."}
  "1 b3 5 6")

(define-chord #uuid "6cb2b165-bd51-4c36-a5f1-e64732897bbf"
  :ninth
  {:sufix       "9"
   :explanation "ninth"
   :text        "This is a dominant chord. The most important tones are 1, 3, b7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 b7 9")

(define-chord #uuid "1df9a391-b6a8-46b8-bb6e-5e2ed261022a"
  :maj-ninth
  {:sufix       "maj9"
   :explanation "major ninth"
   :text        "The most important tones are 1, 3, 7 and 9. The 5 can be ignored in the chord."}
  "1 3 5 7 9")

(define-chord #uuid "f8ddfbbf-50d8-46af-a7e5-fc542cc82355"
  :minor-ninth
  {:sufix       "m9"
   :explanation "minor ninth # fifth is the least important tone, it may be ignored"
   :text        "The most important tones are 1, b3, b7 and 9. The 5 can be ignored in the chord."}
  "1 b3 5 b7 9")

(define-chord #uuid "065c90b0-a8dd-4137-9864-dc8b963c1f07"
  :minor-add9
  {:sufix       "m(add9)"
   :explanation "minor with an added 9"}
  "1 b3 5 9")

(define-chord #uuid "29942046-cae8-4647-bee4-969a090ee9b2"
  :minor-flat6
  {:sufix       "mb6"
   :explanation "minor with an added flat 6"}
  "1 b3 5 b6")

(define-chord #uuid "7c6a74ff-b643-419a-b14e-b4d666bf8115"
  :minor-sixth-added9
  {:sufix       "m6/9"
   :explanation "minor sixth with an added 9"}
  "1 b3 5 6 9")

(define-chord #uuid "aa156ab4-2ddb-49dc-bbdf-2f1f45cbd9e2"
  :maj-eleventh
  {:sufix       "maj11"
   :explanation "major eleventh"}
  "1 3 5 7 9 11")

(define-chord #uuid "8816ae91-e81f-4fec-930b-9597fd1c8efb"
  :eleventh
  {:sufix       "11"
   :explanation "dominant 11"}
  "1 3 5 b7 9 11")

(define-chord #uuid "2702633f-8919-4a41-84d5-84daf43c65db"
  :minor-eleventh
  {:sufix       "m11"
   :explanation "minor eleventh"}
  "1 b3 5 b7 9 11")

(define-chord #uuid "ecd08e90-1b69-4c26-8719-d82c1c101b28"
  :thirteen
  {:sufix       "13"
   :explanation "thirteenth. Dominant"}
  "1 3 5 b7 9 11 13")

(define-chord #uuid "2dfba16e-3816-461b-b088-ef2dcb394f43"
  :maj-thirteen
  {:sufix "13"
   :explanation "major thirteenth"}
  "1 3 5 7 9 11 13")

(define-chord #uuid "97465c6d-9c7a-4ee9-9ebd-336913569935"
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
(define-scale #uuid "39af7096-b5c6-45e9-b743-6791b217a3df"
  :major
  "1, 2, 3, 4, 5, 6, 7")

(define-scale #uuid "d091b747-63b9-4db2-9daa-6e9974852080"
  :minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale #uuid "8c0a7209-4ac4-4ec7-b8a5-e4fdaf449ad6"
  :lydian
  "1, 2, 3, #4, 5, 6, 7")

(define-scale #uuid "ee53b723-dc42-41ee-8467-61e5c0af3ffe"
  :ionian
  "1, 2, 3, 4, 5, 6, 7")

(define-scale #uuid "5a5a5751-9213-4116-9ca7-894cc00be2dc"
  :mixolydian
  "1, 2, 3, 4, 5, 6, b7")

(define-scale #uuid "825e9d0e-9b9d-461c-90b3-c2e71c9730aa"
  :dorian
  "1, 2, b3, 4, 5, 6, b7")

(define-scale #uuid "8136955a-a4cd-4978-98f6-4d4ff596b5db"
  :aeolian
  "1, 2, b3, 4, 5, b6, b7")

(define-scale #uuid "dbf44d25-75b9-4608-8e11-ba733b6edfc0"
  :phrygian
  "1, b2, b3, 4, 5, b6, b7")

(define-scale #uuid "862ef869-25e9-47f9-ab5e-8146f8a8b3e6"
  :locrian
  "1, b2, b3, 4, b5, b6, b7")

(define-scale #uuid "ddf306a1-b119-4eda-b3c3-1f5215cbe6d8"
  :harmonic-minor
  "1, 2, b3, 4, 5, b6, 7")

(define-scale #uuid "6d8e0cba-658d-4072-838e-3d50d926ed0f"
  :melodic-minor
  "1, 2, b3, 4, 5, 6, 7")

(define-scale #uuid "2b5857e2-e368-4847-ae91-d385e9286148"
  :natural-minor
  "1, 2, b3, 4, 5, b6, b7")

(define-scale #uuid "e7ad3188-1e4c-4d19-bd4b-99e97213c6f6"
  :pentatonic-major
  "1, 2, 3, 5, 6")

(define-scale #uuid "82751272-7c3a-445e-a589-24c1ad87a30e"
  :pentatonic-minor
  "1, b3, 4, 5, b7")

(define-scale #uuid "bac1ab62-34df-4232-b205-b197d25d8892"
  :pentatonic-blues
  "1, b3, 4, b5, 5, b7")

(define-scale #uuid "2dd35839-9d00-45bd-b4e8-43868aa9836c"
  :pentatonic-neutral
  "1, 2, 4, 5, b7")

(define-scale #uuid "0995cd25-a646-4347-918d-cfc9f06aa9a1"
  :diatonic
  "1, 2, 3, 5, 6")

(define-scale #uuid "c5500473-a4e1-44c3-833b-95d0c840c9e8"
  :diminished
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale #uuid "3df70e72-dd4c-4e91-85b5-13de2bb062ce"
  :mixolydian-blues-hybrid
  "1, 2, b3, 3, 4, b5, 5, 6, b7")

(define-scale #uuid "888bff34-1059-4f11-b5e0-79d6708dd3c7"
  :diminished-half
  "1, b2, b3, 3, b5, 5, 6, b7")

(define-scale #uuid "4bd060d3-6a6a-48d2-910e-4c2962af1f30"
  :diminished-whole
  "1, 2, b3, 4, b5, b6, 6, 7")

(define-scale #uuid "5867c2d8-e2cf-4221-872e-dc2e50507abd"
  :diminished-whole-tone
  "1, b2, b3, 3, b5, b6, b7")

(define-scale #uuid "96b278ae-704e-46d7-9bd3-2b24bcd91d3b"
  :dominant-7th
  "1, 2, 3, 4, 5, 6, b7")

(define-scale #uuid "1694ffaf-7fcd-4447-ada4-42f272e6ae5e"
  :lydian-augmented
  "1, 2, 3, #4, #5, 6, 7")

(define-scale #uuid "6d32a072-cbbc-4b20-ae01-bda5a87071c6"
  :lydian-minor
  "1, 2, 3, #4, 5, b6, b7")

(define-scale #uuid "656b338a-7938-447b-ad89-c478023a21d4"
  :lydian-diminished
  "1, 2, b3, #4, 5, 6, 7")
;; ---------------
;; Scales end
;; ---------------


;; --------------------
;; Chord patterns
;;
;; Matrixes specifies patterns on how chords looks like and where each interval
;; is located in the matrix. This corresponds to how the chord looks like on the
;; guitar fret board. `nil`s are tones on the fret board where no finger is
;; located.
;; --------------------
(define-chord-pattern #uuid "94f5f7a4-d852-431f-90ca-9e99f89bbb9c"
  {:belongs-to :major
   :tuning     guitar}
  "3   -   -   -
   -   1   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(define-chord-pattern #uuid "3a4d8901-1742-4ccb-9fc8-62238fa2f1c0"
  {:belongs-to :major
   :tuning     ukulele}
  "-   -   -   1
   3   -   -   -
   1   -   -   -
   5   -   -   -")

(define-chord-pattern #uuid "7a0a2ac8-5c26-40a5-b5f2-11fecc447f3e"
  {:belongs-to :major
   :tuning     ukulele}
  "1   -   -
   5   -   -
   -   3   -
   -   -   1")

(define-chord-pattern #uuid "cb55c177-c7de-429e-9356-e6d9869805ef"
  {:belongs-to :major
   :tuning     guitar}
  "5   -   -
   -   -   3
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "72b1230b-2546-4380-a746-c6074c8a7278"
  {:belongs-to :major
   :tuning     guitar}
  "1   -   -
   5   -   -
   -   3   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "03f12eab-1220-407f-9fd5-68be0d83f240"
  {:belongs-to :major
   :tuning     guitar}
  "-   -   -   1
   3   -   -   -
   1   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1")

(define-chord-pattern #uuid "f115cc21-bbc0-48df-a0d5-110f52118c4e"
  {:belongs-to :major-plus-5
   :tuning     guitar}
  "1   -   -   -
   -  #5   -   -
   -   3   -   -
   -   -   1   -
   -   -   -  #5
   1   -   -   -")

(define-chord-pattern #uuid "ddba5f7f-831e-4836-85f8-de2cb6f7127f"
  {:belongs-to :minor
   :tuning     ukulele}
  "1   -   -
   5   -   -
  b3   -   -
   -   -   1")

(define-chord-pattern #uuid "750c8d5d-983a-4937-bbe3-0d0104f6ee39"
  {:belongs-to :minor
   :tuning     ukulele}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   5")

(define-chord-pattern #uuid "e5e74566-82e0-48e3-acba-be673c563006"
  {:belongs-to :minor
   :tuning     guitar}
  "1   -   -
   5   -   -
  b3   -   -
   -   -   1
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "9a23d114-5957-4d7c-a196-1f09b6f5c00c"
  {:belongs-to :minor
   :tuning     guitar}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "a0bb5d7a-8f8b-4741-b9c8-478ff5a4cb91"
  {:belongs-to :minor
   :tuning     guitar}
  "-  b3   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern #uuid "32c7a71f-8992-4d1c-a5b8-5607f1e9f409"
  {:belongs-to :dominant-seven
   :tuning     ukulele}
  "-  b7
   3   -
   1   -
   5   -")

(define-chord-pattern #uuid "74846a21-412d-4b7a-a01e-6f798fa42a5f"
  {:belongs-to :dominant-seven
   :tuning     ukulele}
  "-   -   3
   -  b7   -
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "4c264eba-ae0d-4510-a2fc-367a3d94b42a"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "1   -   -
   5   -   -
   -   3   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "15c84786-78e6-41e2-8492-1866f320059e"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "-   -   3
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "37e97549-af79-40a5-850e-8e40e36c9ed7"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "5   -   -
   -   -   3
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "a8249afc-b6aa-4db1-8aed-c2110f0ce1f6"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "1   -   -   -
   -   -   -  b7
   -   3   -   -
   -   -   1   -
   -   -   5   -
   1   -   -   -")

(define-chord-pattern #uuid "639a0d6d-a636-4761-9c43-3563cff792ff"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "-   -   -  b7
   -   -   3   -
   -   -   1   -
   -   -   5   -
   1   -   -   -
   -   -   -   -")

(define-chord-pattern #uuid "e3548c9a-2522-4c01-b838-2932d5ca7eb1"
  {:belongs-to :dominant-seven
   :tuning     guitar}
  "-   -   -
   1   -   -
   -   -  b7
   -   3   -
   -   -   1
   -   -   -")

(define-chord-pattern #uuid "a679b008-d36b-4d29-b660-71b48536be32"
  {:belongs-to :dominant-seven-sharp-nine
   :tuning     guitar}
  "-   -
   -  #9
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern #uuid "9498a64e-49d7-4fbd-af50-91d2dffa44c0"
  {:belongs-to :dominant-seven-flat-nine
   :tuning     guitar}
  "-   -
  b9   -
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern #uuid "45ccc319-651a-49ae-9cd4-6f2f2a995b36"
  {:belongs-to :minor-seven
   :tuning     guitar}
  "
   5   -   -
   -  b3   -
  b7   -   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "899aa830-b2e7-4819-b708-27b59a87345f"
  {:belongs-to :minor-seven
   :tuning     guitar}
  "
   1   -   -
   5   -   -
  b3   -   -
  b7   -   -
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "b009d31f-3b1b-4f69-a4c8-4be4649d67a8"
  {:belongs-to :minor-seven
   :tuning     guitar}
  "
   -  b3   -
   -  b7   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "2f43882c-298d-4e74-a6ad-b85edfb8453e"
  {:belongs-to :minor-seven
   :tuning     guitar}
  "
   -
   5
  b3
  b7
   -
   1")

(define-chord-pattern #uuid "bd8b0fcd-03d9-46da-89ee-22b33ff8ef0b"
  {:belongs-to :fifth
   :tuning     guitar}
  "-  -  -
   -  -  -
   -  -  -
   -  -  1
   -  -  5
   1  -  -")

(define-chord-pattern #uuid "2c99b2d8-564f-4267-8865-2447fc1e3170"
  {:belongs-to :fifth
   :tuning     guitar}
  "
   -   -   -   -
   -   -   -   1
   -   -   5   -
   1   -   -   -
   -   -   -   -
   -   -   -   -")

(define-chord-pattern #uuid "1a40de53-085c-4d7d-ad07-e1df31ab5f96"
  {:belongs-to :diminished-fifth
   :tuning     guitar}
  "-   -   -
   -  b3   -
   -   -   1
   -  b5   -
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "109b77a4-f6fc-48bc-ac77-01876b9f4794"
  {:belongs-to :diminished-fifth
   :tuning     guitar}
  "-   -   -
   -   -   -
   b3  -   -
   -   -   1
   -  b5   -
   1   -   -")

(define-chord-pattern #uuid "83e131cd-18ac-49ca-a050-7a750d53526e"
  {:belongs-to :major-maj-seven
   :tuning     ukulele}
  "7   -   -   -
   -   5   -   -
   -   -   3   -
   -   -   -   1")

(define-chord-pattern #uuid "2601ea29-18be-46ce-8768-04d37f0ede59"
  {:belongs-to :major-maj-seven
   :tuning     guitar}
  "1   -   -
   5   -   -
   -   3   -
   -   7   -
   -   -   5
   1   -   -")

(define-chord-pattern #uuid "14c1aabc-2bde-425c-86dc-eb604473d7bf"
  {:belongs-to :major-maj-seven
   :tuning     guitar}
  "5   -   -
   -   -   3
   -   7   -
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "a2e3e5c3-da29-444e-bfa3-e34b74e0cbec"
  {:belongs-to :major-maj-seven
   :tuning     guitar}
  "-   -   3
   -   -   7
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "81999502-29d7-4564-8549-71c3820a5959"
  {:belongs-to :major-maj-seven
   :tuning     guitar}
  "-   -
   5   -
   -   3
   -   7
   -   -
   1   -")

(define-chord-pattern #uuid "efde58bf-56c3-486d-8c8a-11a071a4d943"
  {:belongs-to :sixth
   :tuning     guitar}
  "1   -   -
   -   -   6
   -   3   -
   -   -   1
   -   -   5
   -   -   -")

(define-chord-pattern #uuid "a0c3b2f9-677f-4c6b-ba4a-e78fb1f3925a"
  {:belongs-to :sixth
   :tuning     guitar}
  "-   -   6
   -   -   3
   -   -   1
   -   -   5
   1   -   -
   -   -   -")

(define-chord-pattern #uuid "e096b9e5-11c8-4dfe-9bfa-3b5eafe298ce"
  {:belongs-to :sixth
   :tuning     guitar}
  "6   -   -
   3   -   -
   1   -   -
   5   -   -
   -   -   3
   6   -   -")

(define-chord-pattern #uuid "81d9db89-6031-413b-aaff-11a626251899"
  {:belongs-to :sixth
   :tuning     guitar}
  "-   1   -
   -   5   -
   -   -   3
   6   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "f9f49ac2-0f40-46a0-ad75-4eb4b2a75d01"
  {:belongs-to :sixth
   :tuning     guitar}
  "-   -   3
   6   -   -
   -   -   5
   1   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "a8e57c65-20a7-4f33-b6ae-6b2e639cd140"
  {:belongs-to :sixth
   :tuning     guitar}
  "3   -   -
   -   1   -
   5   -   -
   -   -   3
   6   -   -
   3   -   -")

(define-chord-pattern #uuid "729fbe2b-e627-4535-9b36-099127f3db9c"
  {:belongs-to :sixth
   :tuning     guitar}
  "-   -   5
   1   -   -
   -   6   -
   -   3   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "72a0e97c-0c7f-4318-8bf0-a6d5f1f19135"
  {:belongs-to :ninth
   :tuning     guitar}
  "-   5
   -   9
   -  b7
   3   -
   -   1
   -   -")

(define-chord-pattern #uuid "4df3ba12-9d2d-4672-ba17-a05d0225ff4f"
  {:belongs-to :ninth
   :tuning     guitar}
  "-   9   -
   -   -  b7
   3   -   -
   -   1   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "4d945cda-73e8-4dcc-b670-297eb91be3a1"
  {:belongs-to :ninth
   :tuning     guitar}
  "-   -
   -   -
   9   -
   -  b7
   3   -
   -   1")

(define-chord-pattern #uuid "ca0174f2-6cef-4a15-a017-a8f764daab2d"
  {:belongs-to :minor-seven-flat-5
   :tuning     guitar}
  "-   -
  b5   -
   -  b3
   -  b7
   -   -
   -   1")

(define-chord-pattern #uuid "4e9d3630-d01d-4f0d-9bd1-d052af31f5b2"
  {:belongs-to :minor-seven-flat-5
   :tuning     guitar}
  "-   -
   -  b3
  b7   -
   -  b5
   1   -
   -   -")

(define-chord-pattern #uuid "74773752-c21e-43ef-8bad-5faefe59b788"
  {:belongs-to :minor-seven-flat-5
   :tuning     guitar}
  "-   1
  b5   -
   -  b3
   -  b7
   -   -
   -   -")

(define-chord-pattern #uuid "fb66c18d-c20a-4f84-b5b9-58e9e072a24c"
  {:belongs-to :minor-seven-flat-5
   :tuning     guitar}
  "-  b3
   -  b7
   -  b5
   1   -
   -   -
   -   -")

(define-chord-pattern #uuid "dabfc0ee-6aa7-49a9-b228-51181cb93488"
  {:belongs-to :minor-seven-flat-5
   :tuning     guitar}
  "-  b5   -
   1   -   -
   -   -  b7
  b3   -   -
   -   -   -
   -   -   -")

(define-chord-pattern #uuid "17d5216c-9592-4c06-8c60-05aba7a5b985"
  {:belongs-to :minor-ninth
   :tuning     guitar}
  "-   -   -
   -   -   9
   -   -  b7
  b3   -   -
   -   -   1
   -   -   -")

(define-chord-pattern #uuid "f6cc1ac8-263a-48d0-a31c-18ce131d03ce"
  {:belongs-to :eleventh
   :tuning     guitar}
  "5
   9
  b7
  11
   1
   -")

(define-chord-pattern #uuid "59764f90-0e30-42a1-a2ea-79d2e1260b45"
  {:belongs-to :eleventh
   :tuning     guitar}
  "-   -   -
  11   -   -
   -   9   -
   -   -  b7
   -   -   -
   -   -   1")

(define-chord-pattern #uuid "144e925c-52c8-4089-b855-efe5bf66d266"
  {:belongs-to :major
   :tuning     mandolin}
  "-   -   -   1
   -   -   3   -
   5   -   -   -
   1   -   -   -")

(define-chord-pattern #uuid "e4db6b64-e5e1-46c1-a8ab-e1a99aa26a85"
  {:belongs-to :major
   :tuning     mandolin}
  "-   -   3
   5   -   -
   1   -   -
   -   -   5")

(define-chord-pattern #uuid "2e35ce27-4afc-4d3e-a16d-bc01b6607d71"
  {:belongs-to :major
   :tuning     mandolin}
  "-   -   -   5
   -   -   -   1
   -   -   3   -
   5   -   -   -")

(define-chord-pattern #uuid "8d4d421d-0996-4183-a09c-7b10d51be8e1"
  {:belongs-to :major
   :tuning     mandolin}
  "1   -   -
   -   -   5
   -   -   1
   -   3   -")

(define-chord-pattern #uuid "44837702-2c92-4b90-adfb-7e5a82799e42"
  {:belongs-to :minor
   :tuning     mandolin}
  "-   -   -   1
   -  b3   -   -
   5   -   -   -
   1   -   -   -")

(define-chord-pattern #uuid "e1ce4515-14fb-48fb-bd7d-c489b7267e55"
  {:belongs-to :major-maj-seven
   :tuning     mandolin}
  "-   -   7
   -   -   3
   5   -   -
   1   -   -")

(define-chord-pattern #uuid "f812e550-0fef-4dd4-86fd-026d324da5b0"
  {:belongs-to :dominant-seven
   :tuning     mandolin}
  "-  b7   -
   -   -   3
   5   -   -
   1   -   -")

(define-chord-pattern #uuid "3974da8c-aad9-48f0-83a4-99b2700d7e56"
  {:belongs-to :minor-seven
   :tuning     mandolin}
  "-  b7
   -  b3
   5   -
   1   -")

;; --------------------
;; Chord patterns end
;; --------------------

;; ---------------
;; Triad patterns
;; ---------------
(define-triad-pattern #uuid "a9acea3e-3069-4c22-9c71-076721597739"
  {:belongs-to :major
   :tuning     guitar
   :order      1}
  "3   -
   -   1
   5   -
   -   -
   -   -
   -   -")

(define-triad-pattern #uuid "b35c4cda-7d75-4936-95c5-dbaad7c4e08c"
  {:belongs-to :major
   :tuning     guitar
   :order      2}
  "5   -   -
   -   -   3
   -   -   1
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern #uuid "e267154d-3f5b-4124-bf2d-06b9e3e4a621"
  {:belongs-to :major
   :tuning     guitar
   :order      3}
  "1   -   -
   5   -   -
   -   3   -
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern #uuid "6e1242cd-2b41-424e-b8b0-b52a818697a1"
  {:belongs-to :major
   :tuning     guitar
   :order      4}
  "-
   3
   1
   5
   -
   -")

(define-triad-pattern #uuid "ccdd8b36-065c-4170-ad4a-5f4086c308e9"
  {:belongs-to :major
   :tuning     guitar
   :order      5}
  "-   -   -
   5   -   -
   -   3   -
   -   -   1
   -   -   -
   -   -   -")

(define-triad-pattern #uuid "a2479488-683d-4daf-8f9f-f2beb7fe3049"
  {:belongs-to :major
   :tuning     guitar
   :order      6}
  "-   -   -
   -   1   -
   5   -   -
   -   -   3
   -   -   -
   -   -   -")

(define-triad-pattern #uuid "2ec51c51-9d7f-498a-b487-3945d943a341"
  {:belongs-to :major
   :tuning     guitar
   :order      7}
  "-   -   -
   -   -   -
   3   -   -
   -   1   -
   -   5   -
   -   -   -")

(define-triad-pattern #uuid "1b83bc79-54d7-4eaf-a900-9873701eb3f4"
  {:belongs-to :major
   :tuning     guitar
   :order      8}
  "-   -   -   -
   -   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1
   -   -   -   -")

(define-triad-pattern #uuid "2c0e915c-5b08-4bb7-aefa-def3d29323e7"
  {:belongs-to :major
   :tuning     guitar
   :order      9}
  "-   -   -
   -   -   -
   1   -   -
   5   -   -
   -   -   3
   -   -   -")

(define-triad-pattern #uuid "066acddd-a338-4032-a6c5-773dbf956238"
  {:belongs-to :major
   :tuning     guitar
   :order      10}
  "-   -   -
   -   -   -
   -   -   -
   1   -   -
   5   -   -
   -   -   3")

(define-triad-pattern #uuid "ddcdee89-b132-496e-a81a-6c50c0307ca7"
  {:belongs-to :major
   :tuning     guitar
   :order      11}
  "-   -
   -   -
   -   -
   3   -
   -   1
   -   5")

(define-triad-pattern #uuid "38a5c7cb-7908-4abe-b767-55b307030cfd"
  {:belongs-to :major
   :tuning     guitar
   :order      12}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   5   -   -   -
   -   -   3   -
   -   -   -   1")

(define-triad-pattern #uuid "9e7ff5b5-8ed6-4d7f-8919-053f3f8787d1"
  {:belongs-to :minor
   :tuning     guitar
   :order      1}
  "5   -   -
   -  b3   -
   -   -   1
   -   -   -
   -   -   -
   -   -   -")

(define-triad-pattern #uuid "4db09dd6-9a44-4a1b-8c0f-6ed82796c8b5"
  {:belongs-to :minor
   :tuning     guitar
   :order      2}
  "1
   5
  b3
   -
   -
   -")

(define-triad-pattern #uuid "010c370f-cb51-4c26-9aa4-ba4f25be8118"
  {:belongs-to :minor
   :tuning     guitar
   :order      3}
  "b3   -   -
    -   -   1
    -   5   -
    -   -   -
    -   -   -
    -   -   -")

(define-triad-pattern #uuid "6ebef587-b755-4c34-80e9-ed6781c5badb"
  {:belongs-to :minor
   :tuning     guitar
   :order      4}
  " -   -
    -   1
    5   -
    -  b3
    -   -
    -   -")

(define-triad-pattern #uuid "cb43eb3e-31b5-4473-ab86-f34efe038297"
  {:belongs-to :minor
   :tuning     guitar
   :order      5}
  " -   -
   b3   -
    -   1
    -   5
    -   -
    -   -")

(define-triad-pattern #uuid "dbb16753-9b14-4fa7-9192-8dd43e8f677e"
  {:belongs-to :minor
   :tuning     guitar
   :order      6}
  " -   -   -
    5   -   -
   b3   -   -
    -   -   1
    -   -   -
    -   -   -")

(define-triad-pattern #uuid "1b671313-f497-41b2-8d58-ee72503d8e15"
  {:belongs-to :minor
   :tuning     guitar
   :order      7}
  " -   -   -   -
    -   -   -   -
    5   -   -   -
    -  b3   -   -
    -   -   -   1
    -   -   -   -")

(define-triad-pattern #uuid "40aa93ae-2bdc-44a0-95a3-d98004cc46d6"
  {:belongs-to :minor
   :tuning     guitar
   :order      8}
  " -   -
    -   -
    1   -
    5   -
    -  b3
    -   -")

(define-triad-pattern #uuid "076078fe-00ec-4471-9ec1-06caedf26a81"
  {:belongs-to :minor
   :tuning     guitar
   :order      9}
  " -   -   -
    -   -   -
   b3   -   -
    -   -   1
    -   -   5
    -   -   -")

(define-triad-pattern #uuid "c92c3a4c-739c-440c-8cfa-70643213f1fd"
  {:belongs-to :minor
   :tuning     guitar
   :order      10}
  " -   -   -
    -   -   -
    -   -   -
   b3   -   -
    -   -   1
    -   -   5")

(define-triad-pattern #uuid "7a046538-822a-47ac-ad1f-f0efdda910d1"
  {:belongs-to :minor
   :tuning     guitar
   :order      11}
  " -   -   -   -
    -   -   -   -
    -   -   -   -
    5   -   -   -
    -  b3   -   -
    -   -   -   1")

(define-triad-pattern #uuid "f2625159-e4bd-45b1-863a-25e4caaebdcd"
  {:belongs-to :minor
   :tuning     guitar
   :order      12}
  " -   -
    -   -
    -   -
    1   -
    5   -
    -  b3")

(define-triad-pattern #uuid "319567c3-4218-4baf-8eea-681a2f9df2e0"
  {:belongs-to :major
   :tuning     mandolin
   :order      1}
  "-   -   -
   -   -   3
   5   -   -
   1   -   -")

(define-triad-pattern #uuid "3bed9889-984e-4f4d-9c57-aa3096e9fa7f"
  {:belongs-to :major
   :tuning     mandolin
   :order      2}
  "-   -   3
   5   -   -
   1   -   -
   -   -   -")

(define-triad-pattern #uuid "87d66805-8b91-4d70-aa33-257ac33b01d1"
  {:belongs-to :major
   :tuning     mandolin
   :order      3}
  "-   -
   -   5
   -   1
   3   -")

(define-triad-pattern #uuid "914ebc94-498a-4d0c-8963-01451c64ff6f"
  {:belongs-to :major
   :tuning     mandolin
   :order      4}
  "-   -   -   -
   -   -   -   1
   -   -   3   -
   5   -   -   -")

(define-triad-pattern #uuid "685f1cf5-5d85-4606-a3c7-eab4da30e993"
  {:belongs-to :minor
   :tuning     mandolin
   :order      1}
  "-   -   -   -
   -  b3   -   -
   5   -   -   -
   1   -   -   -")

(define-triad-pattern #uuid "472cdb95-b8cd-40cd-8575-d571331a0fdd"
  {:belongs-to :minor
   :tuning     mandolin
   :order      2}
  "-  b3   -   -
   5   -   -   -
   1   -   -   -
   -   -   -   -")

(define-triad-pattern #uuid "213c6887-c7ed-4a0c-b70e-422fb31fed90"
  {:belongs-to :minor
   :tuning     mandolin
   :order      3}
  "-   -   -   -
   -   -   -   1
   -  b3   -   -
   5   -   -   -")

(define-triad-pattern #uuid "4ce974f9-99a6-4e89-927a-58ae770c63b1"
  {:belongs-to :minor
   :tuning     mandolin
   :order      4}
  "-   -   -
   -   -   5
   -   -   1
  b3   -   -")

;; ---------------
;; Triad patterns
;; ---------------

;; --------------------
;; Modes
;; --------------------
(define-mode-pattern #uuid "55189945-37fa-4071-9170-b0b068a23174"
  {:belongs-to :ionian
   :tuning     guitar}
  "7   1   -   2
   -   5   -   6
   2   -   3   4
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern #uuid "1aaa72af-7c36-4b87-8e22-b1b4a719ed1b"
  {:belongs-to :ionian
   :string     6
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern #uuid "8e9ee464-1a23-459a-82f7-9cd30728215a"
  {:belongs-to :ionian
   :string     5
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   6   -   7   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern #uuid "cec2cd9d-ae7f-4d3e-8107-aaf22aaf4004"
  {:belongs-to :ionian
   :string     4
   :tuning     guitar}
  "-   -   -   -   -
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "dbc69a09-b3dc-4bfa-a4df-6dd767b65d25"
  {:belongs-to :ionian
   :string     3
   :tuning     guitar}
  "6   -   7   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "900094ba-9561-4bca-8750-f21f47d08c27"
  {:belongs-to :mixolydian
   :tuning     guitar}
  "-   1   -   2   -
   -   5   -   6  b7
   2   -   3   4   -
   6  b7   -   1   -
   3   4   -   5   -
   -   1   -   2   -")

(define-mode-pattern #uuid "1a422a8c-b40c-4c91-9734-b41f437ddc51"
  {:belongs-to :mixolydian
   :string     6
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2")

(define-mode-pattern #uuid "5c63f201-aee6-4ec8-a224-5ed856a0b8ab"
  {:belongs-to :mixolydian
   :string     5
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   6  b7   -   1
   3   4   -   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern #uuid "4422ee55-0d0d-4944-a0b7-f5ba18a6b8fe"
  {:belongs-to :mixolydian
   :string     4
   :tuning     guitar}
  "-   -   -   -   -
   -   6  b7   -   1
   3   4   -   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "66e4daed-4d20-485a-b7fa-954c7c876a51"
  {:belongs-to :mixolydian
   :string     3
   :tuning     guitar}
  "6  b7   -   1
   3   4   -   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "86adb0c7-0f12-46c2-90f1-6634e3da8cf2"
  {:belongs-to :aeolian
   :tuning     guitar}
  "-   1   -   2  b3
   -   5  b6   -  b7
   2  b3   -   4   -
   -  b7   -   1   -
   -   4   -   5  b6
   -   1   -   2  b3")

(define-mode-pattern #uuid "52340c31-5897-42fe-8920-b5ba0b6d9d61"
  {:belongs-to :aeolian
   :string     6
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3")

(define-mode-pattern #uuid "45897120-9d79-4272-baf9-f90b4d3b4fb0"
  {:belongs-to :aeolian
   :string     5
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1   -   2  b3
   -   -   -   -")

(define-mode-pattern #uuid "473802da-be04-4b35-99c4-4f8c2557169c"
  {:belongs-to :aeolian
   :string     4
   :tuning     guitar}
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "228ead56-2275-459d-819d-52ddd40c3370"
  {:belongs-to :aeolian
   :string     3
   :tuning     guitar}
  "-  b7   -   1   -
   -   4   -   5  b6
   1   -   2  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "9c1b21b5-028a-4a0c-a201-d592e3e319d3"
  {:belongs-to :dorian
   :tuning     guitar}
  "-   1   -   2  b3
   -   5   -   6  b7
   2  b3   -   4   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode-pattern #uuid "8d921443-11e6-4bfc-b10e-f017ce748375"
  {:belongs-to :dorian
   :string     6
   :tuning     guitar}
  "-   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3")

(define-mode-pattern #uuid "dd98e6d1-68c7-429a-b8a5-c5c18b898b59"
  {:belongs-to :dorian
   :string     5
   :tuning     guitar}
  "-   -   -   -   -
   -   -   -   -   -
   6  b7   -   1   -
   -   4   -   5   -
   -   1   -   2  b3
   -   -   -   -   -")

(define-mode-pattern #uuid "8168c9d7-6201-4e0c-88b3-f5bcba5d68f1"
  {:belongs-to :dorian
   :string     4
   :tuning     guitar}
  "-   -   -   -
   6  b7   -   1
   4   -   5   -
   1   -   2  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "5bae88ab-d90c-4f4e-8e79-6783aaa51788"
  {:belongs-to :dorian
   :string     3
   :tuning     guitar}
  "6  b7   -   1
   -   4   -   5
   1   -   2  b3
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "24aeb3b1-99dd-46bf-953a-fb21ac41c88e"
  {:belongs-to :phrygian
   :tuning     guitar}
  "1  b2   -  b3
   5  b6   -  b7
  b3   -   4   -
  b7   -   1  b2
   4   -   5  b6
   1  b2   -  b3")

(define-mode-pattern #uuid "3b414277-b301-40cc-bb30-238b82c21098"
  {:belongs-to :phrygian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3")

(define-mode-pattern #uuid "63a3ddd4-2430-451d-90fe-47522e819cb9"
  {:belongs-to :phrygian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode-pattern #uuid "1fd0e509-9f40-4c20-8070-03f96477873c"
  {:belongs-to :phrygian
   :tuning     guitar}
  "-   -   -   -
   -  b7   -   1
   4   -   5  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "a2f11495-c4f9-48b2-b18f-7de5a97763bb"
  {:belongs-to :phrygian
   :tuning     guitar}
  "-  b7   -   1   -
   -   4   -   5  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "cffc762f-b2aa-43bc-9651-29668ab982db"
  {:belongs-to :lydian
   :tuning     guitar}
  "7   1   -   2
  b5   5   -   6
   2   -   3   -
   6   -   7   1
   3   -  b5   5
   -   1   -   2")

(define-mode-pattern #uuid "37e8a1b9-79b6-46bd-ae0b-37b6af92ebeb"
  {:belongs-to :lydian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
   6   -   7   1
   3   -  #4   5
   -   1   -   2")

(define-mode-pattern #uuid "ff997c3d-26e9-4805-8812-e087f468238e"
  {:belongs-to :lydian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   6   -   7   1
   3   -  #4   5
   -   1   -   2
   -   -   -   -")

(define-mode-pattern #uuid "b66957a2-c045-4a48-aa73-7b6014e6b451"
  {:belongs-to :lydian
   :tuning     guitar}
  "-   -   -   -   -
   -   6   -   7   1
   3   -  #4   5   -
   -   1   -   2   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "6c906182-ab7d-4ebe-845f-5f7381c84dc3"
  {:belongs-to :lydian
   :tuning     guitar}
  "6   -   7   1
   3   -  #4   5
   1   -   2   -
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "b8594762-64ed-48f9-bcea-c1ddae24a610"
  {:belongs-to :locrian
   :tuning     guitar}
  "1  b2   -  b3
   -  b6   -  b7
  b3   -   4  b5
  b7   -   1  b2
   4  b5   -  b6
   1  b2   -  b3")

(define-mode-pattern #uuid "5e44c225-8508-472c-8121-03d9b0408e3d"
  {:belongs-to :locrian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3")

(define-mode-pattern #uuid "30fdd92d-4ef5-42a2-80eb-daf2245ec637"
  {:belongs-to :locrian
   :tuning     guitar}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -")

(define-mode-pattern #uuid "d92fed54-eb65-40df-ada3-b707228d782f"
  {:belongs-to :locrian
   :tuning     guitar}
  "-   -   -   -
   -  b7   -   1
   4  b5   -  b6
   1  b2   -  b3
   -   -   -   -
   -   -   -   -")

(define-mode-pattern #uuid "f1e11c06-deda-4458-b020-119dd3713b9c"
  {:belongs-to :locrian
   :tuning     guitar}
  "-  b7   -   1   -
   -   4  b5   -  b6
   1  b2   -  b3   -
   -   -   -   -   -
   -   -   -   -   -
   -   -   -   -   -")

(define-mode-pattern #uuid "81610fe6-98c3-441f-b361-5a268b8dd45a"
  {:belongs-to :mixolydian-blues-hybrid
   :tuning     guitar}
  "-   1   -   2  b3
   -   5   -   6  b7
   2  b3   3   4  b5
   6  b7   -   1   -
   3   4  b5   5   -
   -   1   -   2  b3")
;; --------------------
;; Modes end
;; --------------------

;; ---------------
;; Scales patterns
;; ---------------
(define-scale-pattern #uuid "c2369bd8-e82f-4405-82b7-222b9c2614bd"
  {:belongs-to :pentatonic-blues
   :tuning     guitar
   :order      1}
  "-   -   -   -
   -   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   5   -
   1   -   -  b3")

(define-scale-pattern #uuid "442d3af1-1453-40ed-b5e3-8afc271cb168"
  {:belongs-to :pentatonic-blues
   :tuning     guitar
   :order      2}
  "-   -   -   -
   -   -   -   -
  b7   -   1   -
   4  b5   5   -
   1   -   -  b3
   -   -   -   -")

(define-scale-pattern #uuid "3170e394-9e23-478f-a0f7-5b585dfdaa9c"
  {:belongs-to :pentatonic-blues
   :tuning     guitar
   :order      3}
  "-   -   -   -
   -  b7   -   1
   4  b5   5   -
   1   -   -  b3
   -   -   -   -
   -   -   -   -")

(define-scale-pattern #uuid "fa1c1ba7-2bef-4b1a-90c4-3befa39613ab"
  {:belongs-to :pentatonic-blues
   :tuning     guitar
   :order      4}
  "-  b7   -   1
   -   4  b5   5
   1   -   -  b3
   -   -   -   -
   -   -   -   -
   -   -   -   -")

(define-scale-pattern #uuid "23f304c1-9947-4654-9617-7311991b8fef"
  {:belongs-to :major
   :tuning     ukulele
   :order      1}
  "-   2   -   3   4
   -   6   -   7   1
   3   4   -   5   -
   -   1   -   2   -")

(define-scale-pattern #uuid "bc68e54c-6f15-4147-9dea-691f334a043d"
  {:belongs-to :major
   :tuning     ukulele
   :order      2}
  "6   -   7   1   -
   3   4   -   5   -
   1   -   2   -   -
   -   -   -   -   -")

(define-scale-pattern #uuid "dcad1b25-f92b-4d1e-93d2-7b46a7de874c"
  {:belongs-to :major
   :tuning     mandolin
   :order      1}
  "-   -   -   -   -   -
   -   -   -   -   -   -
   5   -   6   -   7   1
   1   -   2   -   3   4")

(define-scale-pattern #uuid "11a94327-3df5-419b-8c0a-eba0c46ef780"
  {:belongs-to :major
   :tuning     mandolin
   :order      2}
  "6   -   7   1   -   -
   2   -   3   4   -   5
   -   -   -   -   -   1
   -   -   -   -   -   -")

(define-scale-pattern #uuid "10e574ba-dec9-4fbf-a731-1fc0265e3ec4"
  {:belongs-to :major
   :tuning     mandolin
   :order      3}
  "-   -   -   -   -   -
   5   -   6   -   7   1
   1   -   2   -   3   4
   -   -   -   -   -   -")

(define-scale-pattern #uuid "dbe70a92-9ca1-42f9-b5b4-fc43ff08914e"
  {:belongs-to :major
   :tuning     mandolin
   :order      4}
  "5   -   6   -   7   1
   1   -   2   -   3   4
   -   -   -   -   -   -
   -   -   -   -   -   -")
;; ---------------
;; Scales patterns end
;; ---------------
