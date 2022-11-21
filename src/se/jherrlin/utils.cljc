(ns se.jherrlin.utils
  (:require
   #?(:cljs [goog.string.format])
   #?(:cljs [goog.string :as gstring])
   [clojure.string :as str]))



(defn list-insert [elem index lst]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))

(list-insert
 0
 3
 [1 2 3 4])



#?(:cljs
   (defn fformat
     "Formats a string using goog.string.format.
   e.g: (format \"Cost: %.2f\" 10.0234)"
     [fmt & args]
     (apply gstring/format fmt args))
   :clj (def fformat format))

(fformat "%5d" 3)
(fformat "Hello there, %s" "bob")



(defn rotate-until
  "Rotate collection `xs` util `pred`."
  [pred xs]
  {:pre [(fn? pred) (coll? xs)]}
  (let [xs-count (count xs)]
    (->> (cycle xs)
         (drop-while #(not (pred %)))
         (take xs-count)
         (vec))))

(rotate-until
 #(% :f#)
 [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])



(defn take-indexes
  "Take indexes from a collection"
  [coll indexes]
  (mapv
   (fn [index]
     (nth coll index))
   indexes))

(take-indexes
 [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f}]
 [0 3 5])



(defn rotate-matrix
  "Rotate matrix.
  In:  `[[1 2 3] [3 4 5] [6 7 8]]`
  Out: `[[1 3 6] [2 4 7] [3 5 8]]`"
  [matrix]
  (if-not (seq matrix)
    matrix
    (->> matrix
         (apply mapv vector)
         (mapv identity))))

(rotate-matrix
 [["3" nil nil]
  [nil "1" nil]
  ["5" nil nil]
  [nil nil nil]
  [nil nil nil]
  [nil nil nil]])



(defn trim-matrix
  "Trim a matrix left and right by `pred`"
  ([fretboard-matrix]
   (trim-matrix (partial every? nil?) fretboard-matrix))
  ([pred fretboard-matrix]
   (if (empty? fretboard-matrix)
     fretboard-matrix
     (let [rotated (rotate-matrix fretboard-matrix)
           first?  (->> rotated first pred)
           last?   (->> rotated last pred)]
       (cond
         (and first? last?)
         (->> rotated
              (drop 1)
              (drop-last 1)
              (rotate-matrix)
              (trim-matrix pred))
         (and first? (not last?))
         (->> rotated
              (drop 1)
              (rotate-matrix)
              (trim-matrix pred))
         (and (not first?) last?)
         (->> rotated
              (drop-last 1)
              (rotate-matrix)
              (trim-matrix pred))
         :else fretboard-matrix)))))

(trim-matrix
 [["3" nil nil]
  [nil "1" nil]
  ["5" nil nil]
  [nil nil nil]
  [nil nil nil]
  [nil nil nil]])

(trim-matrix
 [[nil "1" nil nil]
  [nil "5" nil nil]
  [nil "b3" nil nil]
  [nil nil nil "1"]
  [nil nil nil "5"]
  [nil "1" nil nil]])



(defn map-matrix
  "Flatten matrix into one dim list and run `f` on each item. The convert it back
  into a matrix."
  [f matrix]
  (->> (apply concat matrix)
       (map f)
       (partition (-> matrix first count))
       (mapv #(mapv identity %))))

(map-matrix
 inc
 [[1]
  [2]
  [3]])
