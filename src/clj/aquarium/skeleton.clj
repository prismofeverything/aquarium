(ns aquarium.skeleton
  (:require [overtone.at-at :as at-at]
            [bifocals.core :as bifocals]))

(def max-seen [1915.3999 1014.8364 4741.41])
(def min-seen [-1687.6051 -2290.808 263.86478])
(def range-seen [3603.005 3305.6444 4477.54522])

(def track-max (atom [(/ -1.0 0.0) (/ -1.0 0.0) (/ -1.0 0.0)]))
(def track-min (atom [(/ 1.0 0.0) (/ 1.0 0.0) (/ 1.0 0.0)]))

(def max-at [2000 1500 5000])
(def min-at [-2000 -2500 0])
(def range-at [4000 4000 5000])

(def history (atom {}))
(def history-length 5)

(defn track-bounds
  [skeletons]
  (doseq [[id joints] skeletons]
    (doseq [[joint at] joints]
      (doseq [[n mx i] (map vector at @track-max (range 3))]
        (when (> n mx)
          (swap! track-max assoc i n)
          (println "max" @track-max)))
      (doseq [[n mn i] (map vector at @track-min (range 3))]
        (when (< n mn)
          (swap! track-min assoc i n)
          (println "min" @track-min))))))

(def pool 
  (at-at/mk-pool))

(defn normalize-at
  [at]
  (let [normal (mapv
                (fn [n min range] 
                  (/ (- n min) range))
                at min-at range-at)]
    (update-in normal [0] -)))

(defn normalize-skeleton
  [skeleton]
  (reduce
   (fn [reconstruct [joint at]]
     (assoc reconstruct joint (normalize-at at)))
   {} skeleton))

(defn normalize-skeletons
  [skeletons]
  (into
   {}
   (map (fn [[id skeleton]] [id (normalize-skeleton skeleton)]) skeletons)))

(defn wake-skeletons
  []
  (bifocals/tick))

(defn track-history
  [history skeletons limit]
  (reduce
   (fn [history [id skeleton]]
     (update-in history [id] #(take limit (conj % skeleton))))
   history skeletons))

(defn at-plus
  [a b]
  (if a
    (map + a b)
    b))

(defn scale-at
  [at scale]
  (map #(/ % scale) at))

(defn smooth-skeletons
  [full-history scope]
  (reduce
   (fn [skeletons [id history]]
     (let [skeleton
           (reduce
            (fn [smooth previous]
              (reduce 
               (fn [smooth [joint at]]
                 (update-in smooth [joint] #(at-plus % at)))
               smooth previous))
            {} (take scope history))]
       (assoc skeletons 
         id (reduce
             (fn [skeleton [joint at]]
               (assoc skeleton joint (scale-at at (min scope (count history)))))
             {} skeleton))))
   {} full-history))

(defn provide-skeletons
  [handler milli]
  (at-at/every 
   milli 
   (fn [] 
     (bifocals/tick)
     (try 
       (let [skeletons @bifocals/skeletons
             present (keys skeletons)
             normal (normalize-skeletons skeletons)]
         (swap! history track-history normal history-length)
         (track-bounds skeletons)
         ;; (handler normal)
         (handler (smooth-skeletons (select-keys @history present) history-length)))
       (catch Exception e (.printStackTrace e))))
   pool))

(defn halt-skeletons
  [every]
  (at-at/stop every))
