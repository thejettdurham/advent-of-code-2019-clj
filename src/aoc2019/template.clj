(ns aoc2019.template
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  "text input mapped to seq of ints"
  (->> "dayN.txt"
       utils/read-res-file
       clojure.string/split-lines
       (map read-string)))

(defn part1 [] identity)

(defn part2 [] identity)

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))