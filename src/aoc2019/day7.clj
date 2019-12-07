(ns aoc2019.day7
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> "day7.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(def phases-1
  (combo/permutations '(0 1 2 3 4)))

(def phases-2
  (combo/permutations '(5 6 7 8 9)))

; Assuming halted machines are never passed input again
(defn run-feedback
  [input phases]
  (loop [machineInputs (update (vec (map vector phases)) 0 #(into % [0]))
         machineStates [[0 input] [0 input] [0 input] [0 input] [0 input]]
         activeMachine 0]
    (let [mIn (nth machineInputs activeMachine)
          [mIp mCode] (nth machineStates activeMachine)
          [output nextState] (intcode/run-cpu mIp mCode mIn true)
          nextMach (mod (inc activeMachine) 5)
          nextInputs (update (assoc machineInputs activeMachine []) nextMach #(into % output))]
      (if (and (nil? nextState) (= activeMachine 4)) (first output)
                 (recur nextInputs (assoc machineStates activeMachine nextState) nextMach)))))

(defn part1 [] (apply max (map #(reduce (fn [acc phase]
                                (intcode/run-cpu input [phase acc])) 0 %) phases-1)))

(defn part2 [] (apply max (map #(run-feedback input %) phases-2)))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))