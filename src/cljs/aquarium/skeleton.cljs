(ns aquarium.skeleton
  (:require [clojure.set :as set]
            [aquarium.geometry :as geometry]))

(def skeletons (atom {}))

(def scale 30)

(defn make-skeleton-molecule
  [color joints [x y z] geometry]
  (let [[obj molecule]
        (reduce
         (fn [[obj molecule] [joint at]] 
           (let [sphere (geometry/make-sphere 
                         at color 0.02 geometry ;; (js/THREE.TetrahedronGeometry. 0.05) 
;;                         {:transparent true :opacity 0.5}
                         )
                 molecule (assoc molecule joint sphere)]
             (.add obj sphere)
             [obj molecule]))
         [(js/THREE.Object3D.) {}] joints)]
    (.set (.-position obj) x y z)
    (.set (.-scale obj) scale scale scale)
    (.set (.-rotation obj) (* -0.5 Math/PI) 0 0)
    [obj molecule]))

(defn update-skeleton-molecule
  [molecule joints]
  (doseq [[joint at] joints]
    (let [sphere (get molecule joint)]
      (geometry/set-sphere-at sphere at)))
  molecule)

(defn receive-skeletons
  [data scene anchor geometry]
  (let [recent-skeletons (:skeletons data)
        missing-skeletons (set/difference (set (keys @skeletons)) (set (keys recent-skeletons)))]
    (doseq [gone missing-skeletons]
      (let [missing (get @skeletons gone)]
        (.remove scene (:obj missing))
        (swap! skeletons dissoc gone)))
    (doseq [[id joints] recent-skeletons]
      (if-let [skeleton (get @skeletons id)]
        (do
          (swap! skeletons update-in [id :molecule] update-skeleton-molecule joints)
          (swap! skeletons update-in [id :joints] (constantly joints)))
        (let [color (geometry/random-color)
              [obj molecule] (make-skeleton-molecule color joints anchor geometry)]
          (.add scene obj)
          (swap! skeletons assoc id 
                 {:joints joints 
                  :molecule molecule 
                  :obj obj 
                  :color color
                  :collisions {:yoyo 111}}))))))

