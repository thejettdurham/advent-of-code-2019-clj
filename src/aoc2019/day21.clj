(ns aoc2019.day21
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day21.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(defn format-instr
  [s]
  (conj (mapv int s) 10))

; springscript memory: 15

(defn part1 [] (let [instrs (apply concat (map format-instr ["NOT A J"
                                                             "NOT B T"
                                                             "OR T J"
                                                             "NOT C T"
                                                             "OR T J"
                                                             "AND D J"
                                                             "WALK"]))]
                 (last (intcode/run-cpu input instrs))))

(defn part2 [] (let [instrs (apply concat (map format-instr ["NOT A T"
                                                             "OR T J"
                                                             "NOT B T"
                                                             "OR T J"
                                                             "NOT C T"
                                                             "OR T J"
                                                             "NOT D T"
                                                             "NOT T T"
                                                             "AND T J"
                                                             "NOT E T"
                                                             "NOT T T"
                                                             "OR H T"
                                                             "AND T J"
                                                             "RUN"]))]
                 (last (intcode/run-cpu input instrs))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))