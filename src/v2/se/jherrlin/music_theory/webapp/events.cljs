(ns v2.se.jherrlin.music-theory.webapp.events
  (:require
   [re-frame.core :as re-frame]
   [reitit.frontend.controllers :as rfc]
   [reitit.frontend.easy :as rfe]
   [v2.se.jherrlin.music-theory.definitions :as definitions]))


(re-frame/reg-fx
 :push-state
 (fn [route]
   (apply rfe/push-state route)))

(re-frame/reg-event-db
 ::initialize-db
 (fn [db _]
   (if db
     db
     {:current-route nil})))

(re-frame/reg-event-fx
 :push-state
 (fn [_ [_ & route]]
   {:push-state route}))

(re-frame/reg-event-db
 ::navigated
 (fn [db [_ new-match]]
   (let [old-match   (:current-route db)
         controllers (rfc/apply-controllers (:controllers old-match) new-match)]
     (assoc db :current-route (assoc new-match :controllers controllers)))))

(re-frame/reg-sub
 :current-route
 (fn [db]
   (:current-route db)))

(re-frame/reg-sub
 :nr-of-frets
 (fn [db [k]]
   (get db k 16)))

(re-frame/reg-event-db
 :nr-of-frets
 (fn [db [k n]]
   (assoc db k
          (let [nr-of-frets (if-not (string? n)
                              n
                              (js/parseInt n))]
            (if (<= nr-of-frets 4)
              4
              nr-of-frets)))))

(re-frame/reg-sub
 :tuning-name
 (fn [db [k]]
   (get db k :guitar)))

(re-frame/reg-event-db
 :tuning-name
 (fn [db [k n]]
   (assoc db k n)))

(re-frame/reg-sub
 :tuning-tones
 (fn [db [k]]
   (get db k definitions/standard-guitar-tuning)))

(re-frame/reg-event-db
 :tuning-tones
 (fn [db [k n]]
   (assoc db k n)))

(re-frame/reg-sub
 :key-of
 (fn [db [k]]
   (get db k :c)))

(re-frame/reg-event-db
 :key-of
 (fn [db [k n]]
   (assoc db k n)))
