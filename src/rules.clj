(ns rules
  (:require [clara.rules :refer
             [defquery defrule fire-rules insert!
              insert-all query retract! mk-session]]
            [clara.rules.accumulators :as acc]
            [clara.tools.inspect :as inspect]))

(def scales
  {:major/c {:id    :major/c
             :label "C Major"
             :notes #{"a" "b" "c" "d" "e" "f" "g"}}})

;; ----- Facts -----
(defrecord Note       [note])
(defrecord Chord      [chord])
(defrecord ScaleNote  [scale note])

;; ----- Rules -----
(defrule chord-am
  [?notes <- (acc/count) :from [Note]]
  [:test (= 3 ?notes)]
  [Note (= "a" note)]
  [Note (= "c" note)]
  [Note (= "e" note)]
  =>
  (insert!
   (map->Chord {:chord "am"})))

;; ----- Queries -----
(defquery chords?   [] [?facts <- Chord])
(defquery notes?    [] [?facts <- Note])

;; ----- Utils -----
(def rules
  [chord-am])

(def queries
  [chords?
   notes?])

(defn new-session
  ([]
   (new-session (concat rules queries)))
  ([rules-and-queries]
   (mk-session rules-and-queries)))

(defn fire-session [session facts]
  (-> session
      (insert-all facts)
      (fire-rules)))

(defn get-query [session query']
  (->> (query session query')
       (map :?facts)
       (map (partial into {}))))

(defn query-session
  "Given a session, return facts.

  Facts under the `:cmds` key a commands."
  [session]
  (let [q (partial get-query session)]
    {:notes  (q notes?)
     :chords (q chords?)}))

(defn run-session
  "Takes a bunch of facts and run the rule engine."
  ([facts]
   (run-session (new-session) facts))
  ([session facts]
   (let [finished-session (fire-session session facts)]
     (assoc (query-session finished-session)
            :session finished-session
            :facts   facts))))

(comment
  (run-session [(->Note "a")
                (->Note "c")
                (->Note "e")])
  )
