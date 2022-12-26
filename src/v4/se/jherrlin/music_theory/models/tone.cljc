(ns v4.se.jherrlin.music-theory.models.tone
  (:require
   [malli.core :as m]))


(def IndexTone
  "Index tone is a tone without the notion of an interval.
  It's always a set that contains one or two interval tones.

  Example: `#{:eb :d#}`"
  [:set
   {:min 1 :max 2}
   [:enum
    :a :a# :ab :b :bb :c :c# :d :d# :db :e :eb :f :f# :g :g# :gb]])

(m/validate IndexTone #{:c#})        ;; => true
(m/validate IndexTone #{:c :d})      ;; => true
(m/validate IndexTone #{:c :d :eb})  ;; => false
(m/validate IndexTone #{:k})         ;; => false

(def IndexTones
  "Index tones are a vector with index tones.

  Example: `[#{:c} #{:eb :d#}]`"
  [:vector
   {:min 1}
   IndexTone])

(m/validate IndexTones [#{:c} #{:eb :d#}]) ;; => true
(m/validate IndexTones [])                 ;; => false
(m/validate IndexTones [#{:k}])            ;; => false

(def valid-index-tone?  (partial m/validate IndexTone))
(def valid-index-tones? (partial m/validate IndexTones))

(valid-index-tone?  #{:d# :eb})
(valid-index-tones? [#{:d# :eb}])



(def IntervalTone
  "Interval tone is a tone within the context of an interval.
  As an index tone can have two tones, the interval helps distinguish them.

  Example: `:eb`"
  [:enum
   :a :a# :ab :b :bb :c :c# :d :d# :db :e :eb :f :f# :g :g# :gb])


(m/validate IntervalTone :c)  ;; => true
(m/validate IntervalTone :k)  ;; => false
(m/validate IntervalTone nil) ;; => false

(def IntervalTones
  "Interval tones are a vector of interval tones.

  Example: `[:c :e :g]`"
  [:vector
   {:min 1}
   IntervalTone])

(m/validate IntervalTones [:c :e :g :eb])  ;; => true
(m/validate IntervalTones [:c :e :g])      ;; => true
(m/validate IntervalTones [:c :k])         ;; => false
(m/validate IntervalTones [])              ;; => false

(def valid-interval-tone?  (partial m/validate IntervalTone))
(def valid-interval-tones? (partial m/validate IntervalTones))

(valid-interval-tone?  :c)
(valid-interval-tones? [:c])


(def ToneData
  [:map
   [:index-tone    IndexTone]
   [:interval-tone IntervalTone]
   [:interval      string?]
   [:semitones     int?]
   [:name/en       string?]])

(def TonesData
  [:vector
   {:min 1}
   ToneData])

(def valid-tone-data?  (partial m/validate ToneData))
(def valid-tones-data? (partial m/validate TonesData))


(m/validate
 TonesData
 [{:semitones 0,
    :name/en "Root",
    :index-tone #{:c},
    :interval-tone :c,
    :interval "1"}])


(def Indexes
  "[0 3 7]"
  [:vector int?])

(m/validate Indexes [0 3 7])



(def Intervals
  "[\"1\" \"3\" \"5\"]"
  [:vector string?])

(m/validate Intervals ["1" "b3" "7"])
