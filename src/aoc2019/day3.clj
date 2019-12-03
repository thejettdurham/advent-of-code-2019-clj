(ns aoc2019.day3
  (:require [aoc2019.utils :as utils]))

(defn get-dest-coord
  "Given an initial xy and instruction, finds the result xy"
  [[x0 y0] instr]
  (let [[op & dS] instr
        dist (read-string (apply str dS))]
   (cond
     (identical? op \L) [(- x0 dist) y0]
     (identical? op \R) [(+ x0 dist) y0]
     (identical? op \U) [x0 (+ y0 dist)]
     (identical? op \D) [x0 (- y0 dist)]
     )))

(defn interp-points
  "Generates an ordered seq of 1-points between x0 and x1"
  [x0 x1]
  (cond
    (== x0 x1) (cons x0 ())
    ; The inc/dec on the first term ensures we don't duplicate initCoord later
    (> x1 x0) (range (inc x0) (inc x1))
    :else (range (dec x0) (dec x1) -1)))

(defn interpolate
  "Generates an ordered seq of 2-points between xy0 and xy1"
  [[x0 y0] [x1 y1]]
  (let [xs (interp-points x0 x1)
        ys (interp-points y0 y1)]
    (for [x xs y ys]
     [x y])))

(defn trace-wire
  "Given a list of wire instructions, creates an interpolated trace of points it walks"
  [wireInstrs]
  (second (reduce (fn [[x0y0 coords] instr]
                    (let [x1y1 (get-dest-coord x0y0 instr)
                          next (interpolate x0y0 x1y1)]
                      [x1y1 (concat coords next)])) [[0, 0] '([0,0])] wireInstrs)))

(defn mdist
  "Calculate manhattan distance from zero of a given point"
  [[x y]] (+ (Math/abs x) (Math/abs y)))

(defn find-steps-to-cross
  "for indexed traces, finds the total steps to hit each intersection point"
  [t1 t2]
  (reduce-kv (fn [acc xy0 steps0]
               (let [steps1 (get t2 xy0)]
                 (if (nil? steps1) acc (conj acc (+ steps0 steps1))))) () t1))

(defn index-trace
  "convert the raw trace into map of point:step"
  [tr]
  (first (reduce-kv (fn [[acc seen] i v]
                      (if (contains? seen v)
                        [acc seen]
                        [(assoc acc v i) (conj seen v)])) [{}, #{}] (vec tr))))

(def input
  "text input mapped to 2 seqs of instrs"
  (->> "day3.txt"
       utils/read-res-file
       clojure.string/split-lines
       (map #(clojure.string/split %1 #","))))

(def part1 (second (sort (map mdist (apply clojure.set/intersection
                                           (map (comp set trace-wire) input))))))

(def part2 (second (sort (apply find-steps-to-cross
                                (map (comp index-trace trace-wire) input)))))

(defn -main
  []
  (do (println (str "Part 1: " part1))
      (println (str "Part 2: " part2))))