(ns aoc2019.template
  (:require [aoc2019.utils :as utils]))

(def input
  "text input mapped to seq of ints"
  (->> "day1.txt"
       utils/read-res-file
       clojure.string/split-lines
       (map read-string)))

(def part1 identity)

(def part2 identity)

(defn -main
  []
  (do (println (str "Part 1: " part1))
      (println (str "Part 2: " part2))))