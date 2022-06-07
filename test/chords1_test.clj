(ns chords1-test
  (:use [chords1])
  (:require [clojure.test :as t]))

(comment
  (remove-ns 'chords1-test)
  )


(t/deftest chords-test
  (t/is (= (chord :c major)              [:c :e  :g]))
  (t/is (= (chord :c major-seven)        [:c :e  :g :a#]))
  (t/is (= (chord :c major-maj-seven)    [:c :e  :g :b]))
  (t/is (= (chord :c major-seven-flat-5) [:c :e  :f# :a#]))

  (t/is (= (chord :c minor)              [:c :d# :g]))
  (t/is (= (chord :c minor-seven)        [:c :d# :g  :a#]))
  (t/is (= (chord :c minor-maj-seven)    [:c :d# :g  :b]))
  (t/is (= (chord :c minor-seven-flat-5) [:c :d# :f# :a#]))

  (t/is (= (chord :c sus2)               [:c :d :g]))
  (t/is (= (chord :c sus4)               [:c :f :g]))


  )
