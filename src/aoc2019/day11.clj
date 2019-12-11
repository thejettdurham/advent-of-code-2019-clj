(ns aoc2019.day11
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day11.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(defn turn-heading-left
  [[x y]]
  [(- y) x])
(defn turn-heading-right
  [[x y]]
  [y (- x)])
(defn move-forward
  [cursor heading]
  (map + cursor heading))

(defn robot-paint
  [input startGrid]
  (loop [[ip mem relBase] [0 input 0]
         grid startGrid
         cursor [0 0]
         heading [0 1]]
    (let [input (get grid cursor 0)
          [[color hCode] nextState] (intcode/run-cpu ip mem relBase [input])
          nextGrid (assoc grid cursor color)
          nextHeading (if (= hCode 0) (turn-heading-left heading) (turn-heading-right heading) )
          nextCursor (move-forward cursor nextHeading)
          halted? (nil? nextState)]
      (if halted? nextGrid (recur nextState nextGrid nextCursor nextHeading)))))

(defn get-canvas
  "Ordered List of grid points to consider"
  [grid]
  (let [sparse-points (keys grid)
        xMin (first (apply min-key first sparse-points))
        xMax (first (apply max-key first sparse-points))
        yMin (second (apply min-key second sparse-points))
        yMax (second (apply max-key second sparse-points))]
    (println xMin xMax yMin yMax)
    (partition (inc (- xMax xMin)) (for [y (range yMax (dec yMin) -1)
                                   x (range xMin (inc xMax))]
                               [x y]))))

(defn print-message
  [grid]
  (let [canvas (get-canvas grid)]
    (s/join "\n" (map (fn [r] (s/join (map (comp utils/print-px #(get grid % 0)) r))) canvas))))

(defn part1 [] (count (robot-paint input {[0 0] 0})))

(defn part2 [] (print-message (robot-paint input {[0 0] 1})))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2:\n" (part2)))))
