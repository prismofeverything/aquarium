(ns aquarium.core
  (:require [aquarium.geometry :as geometry]
            ;; [aquarium.skeleton :as skeleton]
            [aquarium.scene :as scene]
            [aquarium.events :as events]
            [aquarium.connect :as connect]))

(def membrane-parameters
  {:rows 20
   :columns 20
   :baseline 7
   :membrane-start [-10 -10]
   :membrane-end [10 10]
   :camera-position [0 -14 3]
   :up 0.9
   :sphere-radius 0.5
   :hand-strike -0.16
   :hand-amplify 1.08
   :inertia-threshhold 0.01
   :skeleton-anchor [15 -15 25]
   :joint-geometry (js/THREE.TetrahedronGeometry. 0.05)})

(def tall-parameters
  {:rows 40
   :columns 10
   :baseline 7
   :membrane-start [-5 -5]
   :membrane-end [5 5]
   :camera-position [0 -20 3]
   :up 0.9
   :sphere-radius 0.5
   :hand-strike -0.16
   :hand-amplify 1.08
   :inertia-threshhold 0.01
   :skeleton-anchor [15 -15 25]
   :joint-geometry (js/THREE.TetrahedronGeometry. 0.05)})

(defn on-key-down 
  [event]
  (swap! scene/world assoc-in [:keyboard (.-keyCode event)] true))

(defn on-key-up
  [event]
  (swap! scene/world update-in [:keyboard] dissoc (.-keyCode event)))

(defn on-mouse-down 
  [event]
  (.log js/console "???")
  (swap! scene/world update-in [:mouse :down] (constantly true)))

(defn on-mouse-up
  [event]
  (.log js/console "???")
  (swap! scene/world update-in [:mouse :down] (constantly false)))

(defn on-resize
  []
  (let [[width height] (scene/window-size)
        {:keys [camera renderer]} @scene/world]
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (.setSize renderer width height)))

(defn normalize-n
  [n d]
  (- (* 2.0 (/ (float n) d)) 1.0))

(defn interpret-mouse-position
  [x y]
  (let [[w h] (scene/window-size)
        x (normalize-n x w)
        y (* -1 (normalize-n y h))]
    [x y]))

(defn on-mouse-move 
  [event]
  (let [[x y] [(.-clientX event) (.-clientY event)]
        mouse (interpret-mouse-position x y)
        object (scene/pick-object mouse @scene/world)]
    (if (and object (-> @scene/world :mouse :down))
      (do
        (scene/set-material-color object 0xaaaaaa)
        (set! (.-z (.-position object)) (+ (.-z (.-position object)) 5))))
    (swap! scene/world update-in [:mouse :position] (constantly mouse))))

(defn init-connection
  [data])

;; (defn hand-ray
;;   [skeleton hand]
;;   (let [world (.localToWorld (:obj skeleton) (.clone (.-position hand)))
;;         from (.clone world)
;;         _ (.setZ from 200)
;;         to (js/THREE.Vector3. 0 0 -1)
;;         ray (js/THREE.Raycaster. from to)]
;;     ray))

;; (defn hand-rays
;;   [{:keys [molecule] :as skeleton}]
;;   (reduce 
;;    (fn [rays joint]
;;      (assoc rays joint (hand-ray skeleton (get molecule joint))))
;;    {} [:left-hand :right-hand :head :right-shoulder :left-shoulder :right-elbow :left-elbow]))

;; (defn hand-collide
;;   [skeleton sphere threshhold strike amplify]
;;   (let [color (js/parseInt (+ "0x" (.slice (:color skeleton) 1)))]
;;     (scene/set-material-color sphere color)
;;     (if (> threshhold (Math/abs (.-inertia sphere)))
;;       (set! (.-inertia sphere) (+ (.-inertia sphere) strike))
;;       (set! (.-inertia sphere) (* (.-inertia sphere) amplify)))))

;; (defn update-skeletons
;;   [skeletons]
;;   (let [world @scene/world
;;         parameters (:parameters world)
;;         threshhold (:inertia-threshhold parameters)
;;         strike (:hand-strike parameters)
;;         amplify (:hand-amplify parameters)
;;         anchor (:skeleton-anchor parameters)
;;         geometry (:joint-geometry parameters)]
;;     (skeleton/receive-skeletons skeletons (:scene world) anchor geometry)
;;     (doseq [[id skeleton] (deref skeleton/skeletons)]
;;       (let [rays (hand-rays skeleton)
;;             collisions (reduce 
;;                         (fn [collisions [joint ray]]
;;                           (let [spheres (.intersectObjects ray (.-children (:membrane @scene/world)))]
;;                             (if-let [sphere (first spheres)]
;;                               (do
;;                                 (if-not (= (.-object sphere) (get-in skeleton [:collisions joint]))
;;                                   (hand-collide skeleton (.-object sphere) threshhold strike amplify))
;;                                 (assoc collisions joint (.-object sphere)))
;;                               (assoc collisions joint nil))))
;;                         {} rays)]
;;         (swap! skeleton/skeletons update-in [id :collisions] (constantly collisions))))))

(def websocket-handlers
  {:init init-connection
   ;; :skeletons update-skeletons
   })

(def event-handlers
  {:click (fn [event data])})

(def point-position (js/THREE.Vector3. -2500 2500 1500))

(defn init-scene
  [state]
  (let [scene (:scene state)
        parameters membrane-parameters
        look (js/THREE.Vector3. 0 0 0)
        camera (scene/camera (:camera-position parameters) look)
        ;; controls (js/THREE.OrbitControls. camera)
        ambient (scene/ambient-light 0x001111)
        point (scene/point-light {:color 0xffffff :position point-position})
        field (geometry/make-sphere-field 
               (:rows parameters) 
               (:columns parameters) 
               (:membrane-start parameters)
               (:membrane-end parameters)
               (:sphere-radius parameters) 
               (:baseline parameters))
        membrane (js/THREE.Object3D.)]
    (doseq [{:keys [sphere]} (vals field)]
      (.add membrane sphere))
    (.add scene membrane)
    (.add scene ambient)
    (.add scene point)
    (assoc state
      :camera camera
      ;; :controls controls
      :look look
      :up (:up parameters)
      :field field
      :mouse {:down false :position [0 0]}
      :keyboard {}
      :membrane membrane
      :parameters parameters
      :lights {:ambient ambient :point point})))

(defn update-scene
  [state]
  (let [{:keys [lights camera time keyboard look controls up parameters]} state
        field (geometry/transient-force-cycle (:field state) (:baseline parameters))
        up (if (get keyboard 38) (+ up 0.01) up)
        up (if (get keyboard 40) (- up 0.01) up)
        look-y (* 50 (Math/sin up))
        look-z (- 50 (* 50 (Math/cos up)))]
    ;; (.update controls)
    (.set look 0 look-y look-z)
    (.lookAt camera look)
    ;; (doseq [[id skeleton] @skeleton/skeletons]
    ;;   (doseq [[joint sphere] (:molecule skeleton)]
    ;;     (set! (.-y (.-rotation sphere)) (+ 0.02 (.-y (.-rotation sphere))))))
    (.set
     (.-position (:point lights))
     (* (.-x point-position) (js/Math.cos (* time 2 0.1)))
     (* (.-y point-position) (js/Math.sin (* time 3 0.1)))
     (* (.-z point-position) (js/Math.sin (* time 5 0.1))))
    (assoc state :field field :up up)))

(def on-load
  (set!
   (.-onload js/window)
   (fn []
     (events/init-websockets event-handlers websocket-handlers)
     (events/init-listeners
      {:key-down on-key-down
       :key-up on-key-up
       :mouse-down on-mouse-down
       :mouse-up on-mouse-up
       :mouse-move on-mouse-move
       :resize on-resize})
     (scene/start init-scene update-scene))))

(connect/connect)

