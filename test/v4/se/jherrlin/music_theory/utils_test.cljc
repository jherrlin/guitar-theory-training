(ns v4.se.jherrlin.music-theory.utils-test
  (:require [v4.se.jherrlin.music-theory.utils :as utils]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


(def all-tones utils/all-tones)
