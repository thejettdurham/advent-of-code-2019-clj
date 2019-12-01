(ns aoc2019.day1
  (:require [aoc2019.utils :as utils]))

(defn mass-to-fuel
  "Given a module mass, calculates total required fuel, ignoring fuel weight"
  [in]
  (- (int (Math/floor (/ in 3))) 2))

; I have to imagine there's a better way of doing this than resorting to an
; infinite seq, but alas it evaded me
(defn all-fuel-amts
  "Calculates the seq of required positive fuel amounts for a given module mass"
  ([n] (take-while #(>= %1 0)
                   (lazy-seq (cons n (all-fuel-amts (mass-to-fuel n)))))))

(defn total-mass-to-fuel
  "Given a module mass, calculates total required fuel, accounting for fuel weight"
  [in]
  (reduce + (all-fuel-amts (mass-to-fuel in))))

(def input
  "text input mapped to seq of ints"
  (->> "day1.txt"
       utils/read-res-file
       clojure.string/split-lines
       (map read-string)))

(def part1 (reduce + (map mass-to-fuel input)))

(def part2 (reduce + (map total-mass-to-fuel input)))

(defn -main
  []
  (do (println (str "Part 1: " part1))
      (println (str "Part 2: " part2))))