(ns aoc2019.day17
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day17.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

; screen drawn from 0 0 to end, break on char 10
; Run program with (some input) to draw screen.
; PART 1: Draw screen, then Find every # point that is fully surrounded by #, then sum coords
;  - Build map as 2D matrix, then iterate over each point, collecting (+ x y) if matches pred

(defn get-adj-coords
  [[x y]]
  [[x (inc y)]
   [x (dec y)]
   [(inc x) y]
   [(dec x) y]])

(defn get-coord
  [in [x y]]
  (nth (nth in y []) x nil))

(defn get-screen
  [input]
  (map #(s/split % #"")
       (s/split-lines (s/join (map char input)))))

(defn print-grid
  [grid]
  (println (s/join "\n" (map #(s/join %) grid))))

(defn get-alignment
  [screen]
  (let [sum (atom 0)]
    (dorun (keep-indexed (fn [y row]
                           (dorun (keep-indexed (fn [x it]
                                            (if (= it "#")
                                              (let [adj-coords (get-adj-coords [x y])
                                                    surrounds (s/join (map #(get-coord screen %) adj-coords))]
                                                (if (= "####" surrounds)
                                                  (swap! sum + (* x y)))))) row))) screen))
    @sum))

(defn part1 [] (get-alignment (get-screen (intcode/run-cpu input []))))

(defn format-instr
  [s]
  (conj (mapv int s) 10))

; Sorry, you're not going to find much profundity here
; All I did was capture the grid output from part1, print it to the screen,
; and visually brute-force it. In doing so I discovered an algorithm that worked for my solution,
; But I am out of energy to code it up
; - Identify Start and End Points
; - Use DFS to find the shortest path from Start Coord to End Coord (as list of coords)
;   - This bit would be tricky because loops would create an infinite number of solutions: need some insight to eliminate them
; - Build a list of Turn-And-Move instructions for this path
; - Build all possible sublists from instructions (having max instruction length as boundary condition)
; - For each sublist, eliminate duplicates.
; - The sublist that has only 3 duplicates are your A, B, C
; - Go back to that sublist, and reduce it to movement instructions
; - Build the selected sublist and movement instructions into strings
;
; Given how complicated that process sounds, there's gotta be a simpler insight I'm missing
; That, or there is some trick that graphics programmers know that makes this more straightforward than I described.

(defn part2 [] (let [inp (conj (rest input) 2)
                     moves (format-instr "A,B,A,C,A,B,C,A,B,C")
                     A (format-instr "R,12,R,4,R,10,R,12")
                     B (format-instr "R,6,L,8,R,10")
                     C (format-instr "L,8,R,4,R,4,R,6")
                     vid? (format-instr "n")
                     instrs (concat moves A B C vid?)]
                 (last (intcode/run-cpu inp instrs))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))