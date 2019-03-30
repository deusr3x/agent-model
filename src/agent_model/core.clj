(ns agent-model.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def v 5)

(defn make-random-agent []
  (if (< (rand) 0.8) true false))

(defn create-point [x y]
  {:x x
   :y y
   :id (java.util.UUID/randomUUID)
   :human (make-random-agent)})

(defn generate-points [n size]
  (-> (repeatedly n #(create-point (rand-int size) (rand-int size)))
      vec))

(defn setup []
  (q/frame-rate 30)
  (q/background 255)
  {:points (generate-points 30 500)
   :running? true
   :n 30
   :size 500})

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
  (let [x1 (:x x)
        y1 (:y x)
        x2 (:x y)
        y2 (:y y)]
  (distance (- x1 x2) (- y1 y2))))

(defn find-min-index [v]
  (reduce (fn [[min-idx min-val curr-idx] curr-val]
            (if (< curr-val min-val)
              [curr-idx curr-val (inc curr-idx)]
              [min-idx min-val (inc curr-idx)]))
          [-1 Double/POSITIVE_INFINITY 0]
          v))

(defn myrand [a b]
  (+ a (rand (- b a))))

(defn rand-angle []
  (myrand (* -1 Math/PI) Math/PI))

(defn move-loc [p]
  (let [x (:x p)
        y (:y p)
        theta (rand-angle)
        dx (* v (q/cos theta))
        dy (* v (q/sin theta))
        newx (mod (+ x dx) 500)
        newy (mod (+ y dy) 500)]
    (-> p
        (assoc-in [:x] newx)
        (assoc-in [:y] newy))))

(defn check-dist [p points]
  (if (human? p)
    (for [x points]
      (if (and (< (dist-between-agents p x) 10) (not= (:id x) (:id p)) (not (human? x)))
        (-> p
            (assoc-in [:human] false))
        p))
    p))

(defn update-points [points]
  (reduce
   (fn [new-points ind]
     (update-in new-points [ind] move-loc)
     (update-in new-points [ind] check-dist points))
   points
   (range (count points))))

(defn update-state [state]
  (if (:running? state)
    (update-in state [:points] update-points)
    state))

(defn draw-point [p]
  (if (human? p) (q/stroke 0 255 0) (q/stroke 255 0 0))
  (q/point (:x p) (:y p)))

(defn draw-state [{:keys [points] :as state}]
  (q/stroke 0)
  (q/stroke-weight 4)
  (q/background 255)
  (doseq [p points]
    (draw-point p)))

;;(defn check-dist []
;;  (doseq [x peeps]
;;    (if-not (human? @x)
;;      (doseq [y peeps]
;;        (if (and (< (dist-between-agents @x @y) 10) (not= (:id @x) (:id @y)))
;;;;          (println (dist-between-agents @x @y) (:id @x) (:id @y) (:loc @x) (:loc @y))
;;          (dosync (alter y assoc-in [:human] false))
;;          )))))


(q/defsketch agent-model
  :features [:no-bind-output]
  :title "My first"
  :settings #(q/smooth 2)
  :setup setup
  :update update-state
  :draw draw-state
  :size [500 500]
  :middleware [m/fun-mode])
