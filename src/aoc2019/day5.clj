(ns aoc2019.day5
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day5.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(defn part1 [] (last (intcode/run-cpu input [1])))          ; 6745903
(defn part2 [] (last (intcode/run-cpu input [5])))          ; 9168267

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))