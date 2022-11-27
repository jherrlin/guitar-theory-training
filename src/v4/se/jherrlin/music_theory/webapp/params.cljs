(ns v4.se.jherrlin.music-theory.webapp.params
  (:require
   [re-frame.core :as re-frame]
   [clojure.string :as str]
   [malli.core :as m]))


(def PathParams
  [:map {:closed true}
   [:key-of {:optional true} keyword?]
   [:scale  {:optional true} keyword?]
   [:chord  {:optional true} keyword?]
   [:steps  {:optional true} int?]])

(def valid-path-params? (partial m/validate PathParams))

(m/validate
 PathParams
 {:key-of :c})

(def QueryParams
  [:map {:closed true}
   [:as-intervalls     {:optional true} [:emun "t" "f"]]
   [:nr-of-octavs      {:optional true} pos-int?]
   [:nr-of-frets       {:optional true} pos-int?]
   [:interval-to-tone  {:optional true} [:emun "t" "f"]]
   [:highlighted-tones {:optional true} [:emun "t" "f"]]
   ])


(def valid-query-params? (partial m/validate QueryParams))


(def events-
  [{:n :key-of}
   {:n :path-params}
   {:n :query-params}
   {:n :scale}
   {:n :chord}])

(doseq [{:keys [n s e]} events-]
  (re-frame/reg-sub n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))


(defmulti  convert-param (fn [k _] k))
(defmethod convert-param :key-of
  [_ v]
  (-> v
      (str/lower-case)
      (str/replace "sharp" "#")
      (keyword)))
(defmethod convert-param :scale              [_ v] (keyword v))
(defmethod convert-param :chord              [_ v] (keyword v))
(defmethod convert-param :steps              [_ v] (js/parseInt v))
(defmethod convert-param :nr-of-octavs       [_ v] (js/parseInt v))
(defmethod convert-param :nr-of-frets        [_ v] (js/parseInt v))
(defmethod convert-param :highlighted-tones  [_ v] (= "t" v))
(defmethod convert-param :interval-to-tone   [_ v] (= "t" v))
(defmethod convert-param :as-intervalls      [_ v] (= "t" v))






(convert-param :key-of "hejsan")
(convert-param :nr-of-octavs "1")

(->> {:key-of "csharp"}
     (map (fn [[k v]] [k (convert-param k v)]))
     (into {})
     )
