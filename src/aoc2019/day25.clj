(ns aoc2019.day25
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day25.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(defn print-it
  [out]
  (println (map char out)))

; Yeah, so I just played the game. For me, I made a map and only had 8 items to deal with
; The security gate lets you know if you're below or above where you need to be, so it was just a matter
; of brute forcing some combinations until I got it. Sorry if you were looking for something more satisfying
(defn part1 [] (loop [cmd nil
                      [a0 b0 c0] [0 input 0]]
                 (let [[out [a b c]] (intcode/run-cpu a0 b0 c0 cmd)]
                   (print-it out)
                   (flush)
                   (let [cmd (read-line)
                       cmd' (conj (mapv int cmd) 10)]
                   (recur cmd' [a b c])))))

(defn part2 []
  identity)

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))