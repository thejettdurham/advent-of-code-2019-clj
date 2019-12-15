(ns aoc2019.day15
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day15.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

; Movement command is input, always unit distance: 1 | 2 | 3 | 4

; status
; 0 : Hit wall, no pos change
; 1 : Moved in dir
; 2 : Found it , Once I have the coord, build the shortest path to it

; WALK ALGORITHM: From a coord, move each direction leading to an unseen point
; AS WE WALK:
; Build a map of points to output (0 [empty], 1 [wall], 2 [o2sys])
; Track steps taken for each path
; First time we hit the o2Sys, the number of steps needed is part 1's answer

; - get adjacent points from coord
; - Filter seen
; - map to input instr
; - exec cpu for each (get [point, val])
; - add points to grid
; - recur for each non-wall position
; - ASSUME: the grid is bounded. Eventually, every path dead-ends.
; - FINAL OUTPUT: full map of area, containing walls, space, and o2sys (the maze), stepsList

; SOLVE ALGORITHM: From a coord, move in each defined, non-wall point
; AS WE WALK:
; Build a set of all filled points
; Track list steps taken to reach end of each path
; Execute each path until there's nowhere else to go
; When we hit the end of the path, add the number of steps to a list
; At the end of this process, the largest number in list of steps is part 2's answer

(defn get-adjacent-points
  [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn reject-seen-points
  [grid points]
  (filter #(not (contains? grid %)) points))

(def unit->input
  {[1 0] 4
   [-1 0] 3
   [0 1] 1
   [0 -1] 2})

(defn coords->instr
  [init next]
  (unit->input (mapv - next init)))

(defn run
  [input]
  (let [grid (atom {[0 0] 0})
        stepsToGoalAndPoint (atom [])]
    (letfn [(go [[ip mem relBase] coord steps]
              (let [unseen-adj-points (reject-seen-points @grid (get-adjacent-points coord))
                    nexts (map (fn [pt] [pt
                                         (intcode/run-cpu ip mem relBase [(coords->instr coord pt)])]) unseen-adj-points)
                    nextCoord->output (into {} (map (fn [[coord [[out]]]] [coord out]) nexts))]
                (swap! grid #(into % nextCoord->output))
                (dorun (map (fn [[coord' [[out] state']]] (do
                                                            ;(println coord' out steps)
                                                      (if (= out 2) (swap! stepsToGoalAndPoint #(conj % [steps coord'])))
                                                      (if (not= 0 out) (go state' coord' (inc steps))))) (reverse nexts)))))]
      (go [0 input 0] [0 0] 1))
    [@grid (first @stepsToGoalAndPoint)]))

(defn fill
  [grid init]
  (let [filled (atom #{init})
        endPaths (atom [])]
    (letfn [(go [coord steps]
              (let [unseen-adj-points (reject-seen-points @filled (get-adjacent-points coord))
                    unseen-space-pts (reduce (fn [acc x] (let [val (grid x)]
                                                           (if (and (= 1 val) (not (nil? val)))
                                                             (conj acc x) acc))) '() unseen-adj-points)]
                (if (= 0 (count unseen-space-pts))
                  (swap! endPaths #(conj % steps))
                  (dorun (map (fn [coord'] (do (swap! filled #(conj % coord'))
                                               (go coord' (inc steps)))) unseen-space-pts)))))]
      (go init 0))
    (apply max @endPaths)))

(defn -main
  []
  (let [[grid [steps point]] (run input)] (println (str "Part 1: " steps))
      (println (str "Part 2: " (fill grid point)))))