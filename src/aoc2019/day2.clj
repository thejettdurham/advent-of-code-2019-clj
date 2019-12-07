(ns aoc2019.day2
  (:require [aoc2019.utils :as utils]))

(defn make-instrs [in] (partition 4 in))

(defn parse-instr
  [mem ip]
  (let [instr (nth (make-instrs mem) ip)
        [opC aN aY aZ] instr
        x (nth mem aN)
        y (nth mem aY)]
      (cond
          (= opC 1) (assoc mem aZ (+ x y))
          (= opC 2) (assoc mem aZ (* x y))
          (= opC 99) (reduced (first mem)))
    ))

(def input
  "text input mapped to seq of ints"
  (->> "day2.txt"
       utils/read-res-file
       (#(clojure.string/split %1 #","))
       (map read-string)))

(defn run-pgm-for-input [n v]
  (reduce parse-instr (assoc (vec input) 1 n 2 v)
          ; This takes advantage of the fact that execution is purely sequential
          ; If there was a jump opcode, we'd need an entirely different approach
          (range 0 (count input))))

(def part1 (run-pgm-for-input 12 2))

(def nvs (utils/cartesian [(range 0 100) (range 0 100)]))
(def part2 (reduce (fn [acc [n v]]
                     (let [out (run-pgm-for-input n v)]
                       (if
                         (= out 19690720)
                         (reduced (+ (* 100 n) v))
                         acc))) 0 nvs))

(defn -main
  []
  (do (println (str "Part 1: " part1))
      (println (str "Part 2: " part2))))