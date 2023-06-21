(ns v2.se.jherrlin.music-theory.utils-test
  (:require
   #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
   [v2.se.jherrlin.music-theory.utils :as sut]))

(t/deftest list-insert
  (t/is
   (=
    (sut/list-insert
     0
     3
     [1 2 3 4])
    '(1 2 3 0 4))))

(t/deftest rotate-until
  (t/is
   (=
    (sut/rotate-until
     (fn [x] (x :f#))
     [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])
    [#{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b} #{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f}])))

(t/deftest sharp-or-flat
  (t/is (= (sut/sharp-or-flat #{:g}       "3#")  :g))
  (t/is (= (sut/sharp-or-flat #{:bb :a#}  "3b")  :bb))
  (t/is (= (sut/sharp-or-flat #{:bb :a#}  "3#")  :a#))
  (t/is (= (sut/sharp-or-flat #{:bb :a#}  "3")   :a#)))

(t/deftest juxt-indexes
  (t/is
   (=
    ((sut/juxt-indexes
      [0 3 7])
     [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])
    [#{:c} #{:d# :eb} #{:g}])))

(t/deftest tones-on-indexes
  (t/is
   (=
    (sut/tones-on-indexes
     [0 3 7]
     [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])
    [#{:c} #{:d# :eb} #{:g}])))

(t/deftest juxt-indexes-and-intervals
  (t/is
   (=
    ((sut/juxt-indexes-and-intervals
      [0 3 7]
      ["1" "b3" "5"])
     [#{:c} #{:db :c#} #{:d} #{:d# :eb} #{:e} #{:f} #{:gb :f#} #{:g} #{:g# :ab} #{:a} #{:bb :a#} #{:b}])
    [:c :eb :g])))

(t/deftest intevals-string->intervals-matrix
  (t/is
   (=
    (sut/intevals-string->intervals-matrix
     "3   -
   -   1
   5   -
   -   -
   -   -
   -   -")
    [["3" nil] [nil "1"] ["5" nil] [nil nil] [nil nil] [nil nil]])))
