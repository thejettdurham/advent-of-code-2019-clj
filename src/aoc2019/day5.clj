(ns aoc2019.day5
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  (->> "day5.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(def pMode->getter {\0 nth
                    \1 (fn [_ %2] (identity %2))})

(defn parse-cplx-op
  "Parse a complex op, yielding [opCode, x-getter, y-getter]"
  [op]
  (let [[opCS pModes] (split-at 2 (s/reverse (str op)))
        [xGtr yGtr] (map #(get pMode->getter % nth) pModes)
        opC (read-string (str "10r" (s/join (reverse opCS))))]
    ; default each getter to `nth`, as the pModes aren't always exhaustive
    [opC (or xGtr nth) (or yGtr nth)]))

(defn calc-op-vals
  "Given current memory and instruction pointer,
  Calculates [opcode, next instruction pointer, x value, y value, and z register]"
  [mem ip]
  (let [opCR (nth mem ip)]
    (if (= opCR 99)
      [99 0 [nil nil nil]]
      (let [rX (nth mem (+ 1 ip))
            rY (nth mem (+ 2 ip))
            rZ (nth mem (+ 3 ip))
            ; If this computer is reused again in a future task, then
            ; this way of checking the raw opcode may need to change
            [opC xGtr yGtr] (if (<  opCR 10)
                              [opCR nth nth]
                              (parse-cplx-op opCR)) ]
        ;(println "magic" ip [opCR opC] [xGtr rX] [yGtr rY] rZ)
        (cond
          (= opC 1) [1 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) rZ]]
          (= opC 2) [2 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) rZ]]
          (= opC 3) [3 (+ 2 ip) [nil nil rX]]
          (= opC 4) [4 (+ 2 ip) [(xGtr mem rX) nil nil]]
          (= opC 5) [5 (+ 3 ip) [(xGtr mem rX) (yGtr mem rY) nil]]
          (= opC 6) [6 (+ 3 ip) [(xGtr mem rX) (yGtr mem rY) nil]]
          (= opC 7) [7 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) rZ]]
          (= opC 8) [8 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) rZ]])))))

(defn run-cpu
  "Runs the cpu for a given input and starting value"
  [input startVal]
  (loop [ip 0
         mem (vec input)
         output '()]
    (let [[opC nextIp [x y rZ]] (calc-op-vals mem ip)]
      ;(println "part1x" ip output opC nextIp x y rZ)
      (cond
        (= opC 99) (first output)
        (= opC 1) (recur nextIp (assoc mem rZ (+ x y)) output)
        (= opC 2) (recur nextIp (assoc mem rZ (* x y)) output)
        (= opC 3) (recur nextIp (assoc mem rZ startVal) output)
        (= opC 4) (recur nextIp mem (conj output x))
        (= opC 5) (recur (if (not= 0 x) y nextIp) mem output)
        (= opC 6) (recur (if (= 0 x) y nextIp) mem output)
        (= opC 7) (recur nextIp (assoc mem rZ (if (< x y) 1 0)) output)
        (= opC 8) (recur nextIp (assoc mem rZ (if (= x y) 1 0)) output)))))

(defn part1 [] (run-cpu input 1))                            ; 6745903
(defn part2 [] (run-cpu input 5))                            ; 9168267

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))