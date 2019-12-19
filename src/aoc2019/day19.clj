(ns aoc2019.day19
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day19.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(def input-instrs-1 (utils/cartesian [(range 0 50) (range 0 50)]))

(defn fetch-xy
  [coord]
  (first (intcode/run-cpu input coord)))

(defn ship-fits-at-coord [x y] (= 1 (fetch-xy [(+ x 99) (- y 99)])))



(defn part1 [] (count (filter #(= % 1)
                         (map #(fetch-xy %) input-instrs-1))))

; The trick: take the first point and jump ahead 99 to the right.
; Find the first row that has this point in the beam
; From the first point in that row, the answer is 99 points below.
(defn part2 []
  (loop [x 0 y 100]
    (if (= 1 (fetch-xy [x y]))
      (if (ship-fits-at-coord x y) (+ (* 10000 x) (- y 99))
                                   (recur x (inc y)))
      (recur (inc x) y))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))