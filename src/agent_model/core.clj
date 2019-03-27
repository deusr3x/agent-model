(ns agent-model.core
  (:require [quil.core :as q]))

(defn setup []
  (q/frame-rate 30)
  (q/background 255))

(defrecord Point [x y])

(defrecord Agent [id human loc])

(defn create-point [x y]
  "Creates a point (x, y)"
  (->Point x y))

(defn make-random-agent []
  "Makes a random agent"
  (if (< (rand) 0.8) true false))

(defn create-agent [isperson]
  "Create an agent with true/false as an argument"
  (let [id (java.util.UUID/randomUUID)
        human isperson
        loc (create-point (rand) (rand))]
    (ref (->Agent id human loc))))

(defn create-quil-agent [x y]
  (let [id (java.util.UUID/randomUUID)
        human (make-random-agent)
        loc (create-point x y)]
    (ref (->Agent id human loc))))

(defn human? [agent]
  "Is the agent a human"
  (:human agent))

(defn square [x]
  "Square number"
  (* x x))

(defn distance [x y]
  "Calculte distance between 2 points"
  (Math/sqrt (+ (square x) (square y))))

(defn dist-between-agents [x y]
  "Find the distance between 2 agents"
  (let [x1 (get-in x [:loc :x])
        y1 (get-in x [:loc :y])
        x2 (get-in y [:loc :x])
        y2 (get-in y [:loc :y])]
  (distance (- x1 x2) (- y1 y2))))

(defn print-attribute [agent key]
  "Print the value of a given key for an agent"
  (println (key agent)))

(defn find-min-index [v]
  (reduce (fn [[min-idx min-val curr-idx] curr-val]
            (if (< curr-val min-val)
              [curr-idx curr-val (inc curr-idx)]
              [min-idx min-val (inc curr-idx)]))
          [-1 Double/POSITIVE_INFINITY 0]
          v))

;; Below - testing functions

(def people
  (loop [i 0
         x []]
    (if (< i 10)
      (recur (inc i) (conj x (create-agent (make-random-agent))))
      x)))

(def peeps
  (loop [i 0
         x []]
    (if (< i 30)
      (recur (inc i) (conj x (create-quil-agent (rand-int 500) (rand-int 500))))
      x)))

;;(doseq [x people] (println (dist-between-agents @(people 3) @x)))
;;(doseq [x people] (println (human? x)))

(def v 5)


(defn update-loc [p]
  (let [x (rand-int q/width)
        y (rand-int q/width)]
    (dosync (alter p assoc-in [:loc] (create-point x y)))))

(defn myrand [a b]
  (+ a (rand (- b a))))

(defn rand-angle []
  (myrand (* -1 Math/PI) Math/PI))

(defn move-loc [p]
  (let [x (:x (:loc p))
        y (:y (:loc p))
        theta (rand-angle)
        dx (* v (q/cos theta))
        dy (* v (q/sin theta))]
    (dosync
     (alter p update-in [:loc :x] + dx)
     (alter p update-in [:loc :y] + dy))))

(defn move-loc2 [p]
  (let [x (:x (:loc @p))
        y (:y (:loc @p))
        theta (rand-angle)
        dx (* v (q/cos theta))
        dy (* v (q/sin theta))
        newx (mod (+ x dx) 500)
        newy (mod (+ y dy) 500)]
    (dosync
     (alter p assoc-in [:loc] (create-point newx newy)))))

(defn draw-point [p]
  (if (human? p) (q/stroke 0 255 0) (q/stroke 255 0 0))
  (let [x (:x (:loc p))
        y (:y (:loc p))]
    (q/point x y)))

(defn update-state []
  (doseq [x peeps] (move-loc2 x)))
;;  (move-loc (peeps 1)))

(defn check-dist []
  (doseq [x peeps]
    (if-not (human? @x)
      (doseq [y peeps]
        (if (and (< (dist-between-agents @x @y) 10) (not= (:id @x) (:id @y)))
;;          (println (dist-between-agents @x @y) (:id @x) (:id @y) (:loc @x) (:loc @y))
          (dosync (alter y assoc-in [:human] false))
          )))))

(defn draw []
  (q/stroke 0)
  (q/stroke-weight 4)
  (q/background 255)
  (update-state)
  (check-dist)
  (doseq [p peeps] (draw-point @p)))

(q/defsketch agent-model
  :features [:no-bind-output]
  :title "My first"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [500 500])
