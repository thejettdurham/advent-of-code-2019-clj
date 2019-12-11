(ns aoc2019.day9
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(defn read-instrs
  [in]
  (->> in
       (#(s/split %1 #","))
       (map read-string)))

(def input
  (->> "day9.txt"
       utils/read-res-file
       read-instrs))

(defn part1 [] (intcode/run-cpu input [1]))

(defn part2 [] (intcode/run-cpu input [2]))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))
