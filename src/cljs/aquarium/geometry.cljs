(ns aquarium.geometry
  (:require [clojure.set :as set]))

(def sqrt-three-over-two (/ (Math/sqrt 3.0) 2))

(defn object
  [m]
  (let [out (js-obj)]
    (doall (map #(aset out (name (first %)) (second %)) m))
    out))

(defn random-color
  []
  (let [base (str (.toString (js/Math.random) 16) "000000")]
    (str "#" (.slice base 2 8))))

(defn set-sphere-at
  [sphere [x y z]]
  (.set (.-position sphere) x y z))

(defn make-sphere
  ([at color radius] (make-sphere at color radius (js/THREE.SphereGeometry. radius 16 16)))
  ([at color radius geometry] (make-sphere at color radius geometry {}))
  ([at color radius geometry options]
     (let [material (js/THREE.MeshPhongMaterial. (object (merge {:color (or color 0xff1493)} options)))
           sphere (js/THREE.Mesh. geometry material)]
       (set-sphere-at sphere at)
       sphere)))

(defn find-surrounding-indexes
  [[row col]]
  (let [skew (if (even? row) 0 -1)]
    [[(dec row) (+ col skew)] 
     [(dec row) (+ col skew 1)] 
     [row (dec col)] 
     [row (inc col)] 
     [(inc row) (+ col skew)] 
     [(inc row) (+ col skew 1)]]))

(defn find-neighbors
  [field [index _]]
  (update-in 
   field [index]
   (fn [sphere]
     (let [surrounding (find-surrounding-indexes index)
           neighbors (remove 
                      nil? 
                      (map
                       (fn [neighbor-index]
                         (if-let [neighbor (get field neighbor-index)]
                           (get neighbor :sphere)))
                       surrounding))]
       (assoc sphere :neighbors (vec neighbors))))))

(defn make-sphere-field
  [rows cols [start-x start-y] [end-x end-y] radius z]
  (let [width (- end-x start-x)
        height (- end-y start-y)
        single-width (/ width cols)
        geometry (js/THREE.SphereGeometry. radius 16 16)
        field (into 
               {}
               (mapcat
                (fn [row]
                  (map 
                   (fn [col]
                     (let [y-portion (float (/ row rows))
                           x-portion (float (/ col cols))
                           at [(+ (* x-portion width) start-x (if (even? row) (* 0.5 single-width) 0))
                               (+ (* y-portion width sqrt-three-over-two) start-y)
                               ;; (if (= [row col] [0 0]) (* 10 z) z)
                               z]
                           sphere (make-sphere at 0x666666 radius geometry)] ;; 0xaa1133
                       (set! (.-inertia sphere) 0)
                       [[row col] {:sphere sphere :inertia 0}]))
                   (range cols)))
                (range rows)))
        field (reduce find-neighbors field field)]
    field))

(def pull-strength 0.0005)
(def return-strength 0.0005)
(def dampening-strength 0.995)

;; (def pull-strength 0.001)
;; (def return-strength 0.001)
;; (def dampening-strength 0.995)

;; (def pull-strength 0.0003)
;; (def return-strength 0.0005)
;; (def dampening-strength 0.99)

(defn impose-force
  [orb baseline index]
  (let [sphere (get orb :sphere)
        z (.-z (.-position sphere))
        pull (reduce
              (fn [pull other]
                (+ pull (- (.-z (.-position other)) z)))
              0 (:neighbors orb))
        return (- baseline z)
        balance (+ (* pull pull-strength) (* return return-strength))
        inertia (* dampening-strength (+ (.-inertia sphere) balance))]
    (set! (.-inertia sphere) inertia)
    orb))

(defn balance-force
  [orb]
  (let [position (.-position (:sphere orb))
        inertia (.-inertia (:sphere orb))]
    (.setZ position (+ (.-z position) inertia))
    orb))

(defn force-cycle
  [field baseline]
  (let [field (map 
               (fn [[index orb]]
                 [index (impose-force orb baseline index)])
               field)]
    (into
     {}
     (map 
      (fn [[index orb]]
        [index (balance-force orb)])
      field))))

(defn transient-force-cycle
  [field baseline]
  (let [indexes (keys field)
        trans-field (transient field)
        trans-field (loop [field trans-field
                           indexes indexes]
                      (if (empty? indexes)
                        field
                        (let [index (first indexes)
                              orb (get field index)
                              orb (impose-force orb baseline index)]
                          (recur (assoc! field index orb) (rest indexes)))))
        trans-field (loop [field trans-field
                           indexes indexes]
                      (if (empty? indexes)
                        field
                        (let [index (first indexes)
                              orb (get field index)
                              orb (balance-force orb)]
                          (recur (assoc! field index orb) (rest indexes)))))]
    (persistent! trans-field)))

