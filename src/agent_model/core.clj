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
            (if (and (< curr-val min-val) (not= curr-val 0.0))
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
    (for [x points]
      (if (and (< (dist-between-agents p x) 10) (not= (:id x) (:id p)) (not (human? x)))
        (-> p
            (assoc-in [:human] false))
        ))
    )

(defn get-dist [p points]
  (for [x points]
    (if (:human x)
      (dist-between-agents p x)
      0.0)))

(defn get-min-point [v]
  (nth (find-min-index v) 0))

(defn get-min-index [p points]
  (->> (get-dist p points)
       (get-min-point)))

(defn move-toward-point [p target]
  (let [x1 (:x p)
        y1 (:y p)
        x2 (:x target)
        y2 (:y target)
        theta (q/atan2 (- y2 y1) (- x2 x1))
        dx (* v (q/cos theta))
        dy (* v (q/sin theta))
        newx (mod (+ x1 dx) 500)
        newy (mod (+ y1 dy) 500)]
    (-> p
        (assoc-in [:x] newx)
        (assoc-in [:y] newy))))

(defn in-range? [x]
  (if (< x 10) true false))

(defn is-same? [a b]
  (if (= (:id a) (:id b)) true false))

(defn find-points [points x y]
  (for [ind (range(count points))
        :let [p (points ind)]
        :when (< (q/dist (:x p) (:y p) x y) 50)]
    ind))

(defn get-points [p points]
  (let [x (check-dist p points)
        y (remove nil? x)
        z (take 1 y)]
    (if (empty? z) p (nth z 0))))

(defn move [p points]
  (if (not (:human p))
    (move-toward-point p (points (get-min-index p points)))
    (move-loc p)))

(defn update-points [points]
  (reduce
   (fn [new-points ind]
     (-> (update-in new-points [ind] move points)
         (update-in [ind] get-points points)))
   points
   (range (count points))))

(defn update-running [state]
  (if (empty? (filter human? (:points state)))
    false
    true))

(defn print-running [state]
  (prn (:running? state)))

(defn update-state [state]
  (if (update-running state)
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

(q/defsketch agent-model
  :features [:no-bind-output]
  :title "My first"
  :settings #(q/smooth 2)
  :setup setup
  :update update-state
  :draw draw-state
  :size [500 500]
  :middleware [m/fun-mode])
