(ns aquarium.lsystem)

(defn condition
  [pre body post]
  {:pre pre
   :body body
   :post post})

(defn production
  [condition result]
  {:condition condition
   :result result})

(defn lsystem
  [symbols productions]
  {:symbols symbols
   :productions productions})

(defn all?
  [predicate & args]
  (every? predicate (apply (partial map vector) args)))

(defn match
  [[archetype scrutiny]]
  (or 
   (= archetype scrutiny)
   (= archetype :*)))

(defn scan
  [[template fragment]]
  (all? match template fragment))

(defn coincides?
  [seed past {:keys [pre body post]}]
  (let [window (count body)
        before (reverse (take (count pre) past))
        here (take window seed)
        after (take (count post) (drop window seed))]
    (all? scan [before here after] [pre body post])))

(defn transform
  [seed past {:keys [condition result]}]
  (if (coincides? seed past condition)
    [true result]
    [false seed]))

(defn produce
  [seed past future productions]
  (loop [productions productions]
    (if (empty? productions)
      (conj future (first seed))
      (let [production (first productions)
            [match? result] (transform seed past production)]
        (if match?
          (conj future result)
          (recur (rest productions)))))))

(defn iterate-system
  [{:keys [symbols productions]} seed]
  (apply
   concat
   (loop [past nil
          future []
          seed seed]
     (if (empty? seed)
       future
       (let [future (produce seed past future productions)]
         (recur (cons (first seed) past) future (rest seed)))))))

(defn travel
  [system seed]
  (iterate (partial iterate-system system) seed))

(defn testl
  []
  (let [p1 (production (condition nil [:a] nil) [:a :b])
        p2 (production (condition nil [:b] nil) [:b :a :b])]
    (travel (lsystem nil [p1 p2]) '(:a))))
