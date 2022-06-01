(ns chords-test
  (:require [chords :as c]
            [clojure.test :as t]))

(comment
  (remove-ns 'chords-test)
  )


(t/deftest chords-test
  (t/is (= (c/major-scale-tones c/tones)                [:c :d :e :f :g :a :b]))
  (t/is (= (c/minor-scale-tones c/tones)                [:c :d :d# :f :g :g# :a#]))

  (t/is (= (c/chord :c c/major)             [:c :e :g]))
  (t/is (= (c/chord :c c/major c/seven)     [:c :e :g :a#]))
  (t/is (= (c/chord :c c/major c/maj-seven) [:c :e :g :b]))

  (t/is (= (c/chord :c c/minor)             [:c :d# :g]))
  (t/is (= (c/chord :c c/minor c/seven)     [:c :d# :g :a#]))
  (t/is (= (c/chord :c c/minor c/maj-seven) [:c :d# :g :b]))

  (t/is (= (c/sus2  c/tones)                [:c :d :g]))
  (t/is (= (c/sus4  c/tones)                [:c :f :g]))
  )
