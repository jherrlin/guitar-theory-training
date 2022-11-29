(ns v4.se.jherrlin.music-theory.webapp.params
  (:require
   [re-frame.core :as re-frame]
   [clojure.string :as str]
   [malli.core :as m]))


(def PathParams
  [:map {:closed true}
   [:key-of      {:optional true} keyword?]
   [:scale       {:optional true} keyword?]
   [:chord       {:optional true} keyword?]
   [:steps       {:optional true} keyword?]
   [:instrument  {:optional true} keyword?]])

(def path-params
  (->> PathParams
       (drop 2)
       (sort)
       (mapv first)))
(def valid-path-params?  (partial m/validate PathParams))
(def explain-path-params (partial m/explain PathParams))

(m/validate
 PathParams
 {:key-of :c})

(def QueryParams
  [:map {:closed true}
   [:as-intervals      {:optional true} :boolean]
   [:nr-of-octavs      {:optional true} pos-int?]
   [:nr-of-frets       {:optional true} pos-int?]
   [:interval-to-tone  {:optional true} :boolean]
   [:highlighted-tones {:optional true} :boolean]
   [:trim              {:optional true} :boolean]
   [:debug             {:optional true} :boolean]
   [:combined-triads   {:optional true} :boolean]])


(def query-params
  (->> QueryParams
       (drop 2)
       (sort)
       (mapv first)))
(def valid-query-params? (partial m/validate QueryParams))
(def explain-query-params (partial m/explain QueryParams))


(->> (concat
      (->> PathParams
           (drop 2)
           (map first)
           (sort)
           (mapv
            (fn [x]
              {:n x
               :s '(fn [db [k]]
                     (get-in db [:path-params k]))
               :e '(fn [db [k v]]
                     (assoc-in db [:path-params k] v))})))
      (->> QueryParams
           (drop 2)
           (map first)
           (sort)
           (mapv
            (fn [x]
              {:n x
               :s '(fn [db [k]]
                     (get-in db [:query-params k]))
               :e '(fn [db [k v]]
                     (assoc-in db [:query-params k] v))}))))
     (vec))

(def events-
  [
   ;; :path-params
   {:n :chord,
    :s (fn [db [k]] (get-in db [:path-params k] :major)),
    :e (fn [db [k v]] (assoc-in db [:path-params k] v))}
   {:n :key-of,
    :s (fn [db [k]] (get-in db [:path-params k] :c)),
    :e (fn [db [k v]] (assoc-in db [:path-params k] v))}
   {:n :scale,
    :s (fn [db [k]] (get-in db [:path-params k] :major)),
    :e (fn [db [k v]] (assoc-in db [:path-params k] v))}
   {:n :steps,
    :s (fn [db [k]] (get-in db [:path-params k] :triad)),
    :e (fn [db [k v]] (assoc-in db [:path-params k] v))}
   {:n :instrument,
    :s (fn [db [k]] (get-in db [:path-params k] :guitar)),
    :e (fn [db [k v]] (assoc-in db [:path-params k] v))}

   ;; :query-params
   {:n :as-intervals,
    :s (fn [db [k]] (get-in db [:query-params k] false)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :highlighted-tones,
    :s (fn [db [k]] (get-in db [:query-params k] true)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :interval-to-tone,
    :s (fn [db [k]] (get-in db [:query-params k] true)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :nr-of-frets,
    :s (fn [db [k]] (get-in db [:query-params k] 16)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :nr-of-octavs,
    :s (fn [db [k]] (get-in db [:query-params k] 2)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :trim,
    :s (fn [db [k]] (get-in db [:query-params k] false)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :debug,
    :s (fn [db [k]] (get-in db [:query-params k] false)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   {:n :combined-triads,
    :s (fn [db [k]] (get-in db [:query-params k] false)),
    :e (fn [db [k v]] (assoc-in db [:query-params k] v))}
   ])

(comment
  (re-frame/dispatch [:debug true])
  (re-frame/dispatch [:debug false])
  )


(defmulti  convert (fn [k _] k))
(defmethod convert :as-intervals       [_ v] (= "true" v))
(defmethod convert :chord              [_ v] (keyword v))
(defmethod convert :highlighted-tones  [_ v] (= "true" v))
(defmethod convert :interval-to-tone   [_ v] (= "true" v))
(defmethod convert :key-of             [_ v] (-> v (str/lower-case) (str/replace "sharp" "#") (keyword)))
(defmethod convert :nr-of-frets        [_ v] (inc (js/parseInt v)))
(defmethod convert :nr-of-octavs       [_ v] (js/parseInt v))
(defmethod convert :scale              [_ v] (keyword v))
(defmethod convert :steps              [_ v] (keyword v))
(defmethod convert :trim               [_ v] (= "true" v))
(defmethod convert :debug              [_ v] (= "true" v))
(defmethod convert :combined-triads    [_ v] (= "true" v))
(defmethod convert :instrument         [_ v] (keyword v))


(defn convert-map
  "Convert values from URL params map."
  [m]
  (->> m
       (map (fn [[k v]] [k (convert k v)]))
       (into {})))

(def events2-
  [{:n :path-params
    :s (fn [db [k]] (get db k))
    :e (fn [db1 [k1 v1]]
         (let [converted (convert-map v1)
               schema    PathParams]
           (if (m/validate schema converted)
             (update db1 k1 merge converted)
             (js/console.log (m/explain schema converted)))))}
   {:n :clear-path-params
    :e (fn [db [_]] (assoc db :path-params {}))}

   {:n :query-params
    :s (fn [db [k]] (get db k))
    :e (fn [db2 [k2 v2]]
         (let [converted (convert-map v2)
               schema    QueryParams]
           (if (m/validate schema converted)
             (assoc db2 k2 converted)
             (js/console.log (m/explain schema converted)))))}
   {:n :clear-query-params
    :e (fn [db [_]] (assoc db :query-params {}))}])


(doseq [{:keys [n s e]} (concat events- events2-)]
  (re-frame/reg-sub      n (or s (fn [db [n']] (get db n'))))
  (re-frame/reg-event-db n (or e (fn [db [_ e]] (assoc db n e)))))




(convert :key-of "hejsan")
(convert :nr-of-octavs "1")
