(ns se.jherrlin.music-theory
  (:require
   [se.jherrlin.music-theory.intervals :as intervals]
   [se.jherrlin.music-theory.utils
    :refer [docstring->m find-chord-name find-root
            fret-table-with-tones juxt-intervals
            find-chord]
    :as utils]
   [clojure.string :as str]))


(comment
  (remove-ns 'se.jherrlin.music-theory)
  )

(def tones [:c :c# :d :d# :e :f :f# :g :g# :a :a# :b])

;; ---------------
;; Partially applied functions.
;; Presets arguments that can be predefined.
;; ---------------
(def chords-atom (atom {}))
@chords-atom
(def scale-atom (atom {}))
@scale-atom

(def find-root-p #(find-root % tones))
(def fret-table-with-tones-p (partial fret-table-with-tones tones))
(defn find-chord-name-p [chord-tones]
  (find-chord-name @chords-atom tones chord-tones))
(defn find-chord-p [chord-tones]
  (find-chord @chords-atom tones chord-tones))




;; ---------------
;; Partial functions end.
;; ---------------
(defn define
  ([state name intervals]
   (define state name {} intervals))
  ([state name meta-data intervals]
   (let [indexes (->> intervals
                      (re-seq #"b{0,2}#{0,2}\d")
                      (mapv #(get-in intervals/intervals-map-by-function [% :semitones])))
         tags    (cond-> #{}
                   (contains? (set indexes) 3)     (conj :minor)
                   (contains? (set indexes) 4)     (conj :major)
                   (str/includes? intervals "bb7") (conj :diminished)
                   (str/includes? intervals "7")   (conj :seventh))]
     (swap! state assoc name
            (assoc meta-data
                   :id name
                   :intervals intervals
                   :indexes indexes
                   :tags tags
                   :f (juxt-intervals indexes))))))

(def define-chord (partial define chords-atom))
(def define-scale (partial define scale-atom))

;; ---------------
;; Chords
;; ---------------

(define-scale :melodic-minor-scale
  "1, 2, b3, 4, 5, 6, 7")

(define-scale :natural-minor-scale
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :pentatonic-major-scale
  "1, 2, 3, 5, 6")

(define-scale :pentatonic-minor-scale
  "1, b3, 4, 5, b7")

(define-scale :pentatonic-blues-scale
  "1, b3, 4, b5, 5, b7")

(define-scale :pentatonic-neutral-scale
  "1, 2, 4, 5, b7")

(define-scale :ionian-scale
  "1, 2, 3, 4, 5, 6, 7")

(define-scale :dorian-scale
  "1, 2, b3, 4, 5, 6, b7")

(define-scale :phrygian-scale
  "1, b2, b3, 4, 5, b6, b7")

(define-scale :lydian-scale
  "1, 2, 3, #4, 5, 6, 7")

(define-scale :mixolydian-scale
  "1, 2, 3, 4, 5, 6, b7")

(define-scale :aeolian-scale
  "1, 2, b3, 4, 5, b6, b7")

(define-scale :locrian-scale
  "1, b2, b3, 4, b5, b6, b7")

(define-scale :diatonic-scale
  "1, 2, 3, 5, 6")

(define-scale :diminished-scale
  "1, 2, b3, 4, b5, b6, 6, 7")


(comment
  @scales

  (diminished-scale
   (find-root-p :e))

  (print
   (fret-table-with-tones-p
    (mixolydian-scale
     (find-root-p :g))))
  )
;; ---------------
;; Scales end
;; ---------------


;; --------------------
;; Harmonization
;; --------------------
(def triad   (juxt #(nth % 0) #(nth % 2) #(nth % 4)))
(def seventh (juxt #(nth % 0) #(nth % 2) #(nth % 4) #(nth % 6)))

(def harmonizations-map
  {:triad   triad
   :seventh seventh})

;; (defn harmonizations [scales-map chords-map all-tones tone scale f]
;;   {:pre [(keyword? tone) (keyword? scale)]}
;;   (let [scale-tones ((get-in scales-map [scale :f]) (find-root tone all-tones))]
;;     (->> scale-tones
;;          (reduce
;;           (fn [m t]
;;             (let [chord-tones (f (find-root t scale-tones))
;;                   chord-name  (find-chord-name chords-map all-tones chord-tones)]
;;               (conj m {:root        tone
;;                        :scale       scale
;;                        :chord-name  chord-name
;;                        :chord-tones chord-tones})))
;;           [])
;;          (mapv
;;           #(assoc %2 :index %1)
;;           (range 1 100)))))

;; (defn harmonizations-p [tone scale f]
;;   (harmonizations @scales @chords tones tone scale f))

;; (harmonizations @scales @chords tones :c :major triad)
;; (harmonizations-p :c :major triad)
;; (harmonizations-p :c :minor triad)





;; --------------------
;; Harmonization end
;; --------------------




;; mode to chord
;; (for [scale (vals @scales)
;;       chord (vals @chords)
;;       :when (set/subset? (set (:indexes chord)) (set (:indexes scale)))]
;;   {:chord chord
;;    :scale scale})
