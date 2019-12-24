(ns aoc2019.day24
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math]))

(defn parse-input
  [in]
  (let [grid (atom {})]
    (dorun (keep-indexed (fn [yi y]
                           (dorun (keep-indexed (fn [xi x]
                                                  (do (swap! grid #(assoc % [xi yi] x)))) y))) in))
    @grid))

(def points
  (for [x (range 0 5)
        y (range 0 5)
        ] [x y]))

(def input
  (->> "day24.txt"
       utils/read-res-file
       clojure.string/split-lines
       parse-input))

(def bio-points
  (into {} (keep-indexed (fn [i coord] [coord (math/expt 2 i)])(for [y (range 0 5)
                                                                                 x (range 0 5)]
                                                                             [x y]))))

(defn get-adj-bugs
  [grid coord]
  (count (filter #(= \# %) (map #(get grid %) (utils/get-adjacent-points coord)))))

(def dim (range 0 5))
(defn get-adj-bugs-3d
  "this will modify the grid to add new levels to consider"
  [grid-3 [x y z :as crd0]]
  (let [adj-plane-coords (utils/get-adjacent-points [x y])
        adj-space-coords (reduce (fn [acc [x' y' :as crd]]
                                   (cond
                                     (neg? x') (conj acc [1 2 (inc z)])
                                     (> x' 4) (conj acc [3 2 (inc z)])
                                     (neg? y') (conj acc [2 1 (inc z)])
                                     (> y' 4) (conj acc [2 3 (inc z)])
                                     (= [2,2] crd) (cond
                                                     (= 1 x) (concat acc
                                                                     (for [i dim]
                                                                       [i 0 (dec z)]))
                                                     (= 3 y) (concat acc
                                                                     (for [i dim]
                                                                       [4 i (dec z)]))
                                                     (= 3 x) (concat acc
                                                                     (for [i dim]
                                                                       [i 4 (dec z)]))
                                                     (= 1 y) (concat acc
                                                                     (for [i dim]
                                                                       [0 i (dec z)])))
                                     :else (conj acc [x' y' z]))) [] adj-plane-coords)
        grid' (merge (into {} (map #(vector % \.) adj-space-coords)) grid-3)
        adj-v (map #(get grid' %) adj-space-coords)]
    (println adj-plane-coords adj-space-coords)
    [grid' (count (filter #(= \# %) adj-v))]))

(defn calc-biodiversity
  [grid]
  (reduce-kv (fn [acc k v]
               (let [bio-val (bio-points k)]
                 (if (= v \#) (+ acc bio-val) acc))) 0 grid))

(defn print-grid
  [g]
  (let [sg (into (sorted-map) g)
        lines (reduce-kv (fn [l [x y] v]
                           (assoc-in l [y x] v )) [[] [] [] [] []] sg)
        s (s/join "\n" (map #(s/join "" %) lines))]
    (println s)))

(defn simulate
  [grid]
  (reduce-kv (fn [grid' coord v]
               (let [adj-bugs (get-adj-bugs grid coord)]
                (assoc grid' coord (if (= v \.)
                                     (if (or (= 1 adj-bugs) (= 2 adj-bugs))  \# \.)
                                     (if (= 1 adj-bugs) \# \.))))) {} grid))

(defn simulate-2
  [grid]
  (reduce-kv (fn [grid' [x y :as coord] v]
               ;(println coord (= [x, y] [2, 2]))
               (if (= [x, y] [2, 2]) grid'
                                    (let [[grid-ext adj-bugs] (get-adj-bugs-3d grid coord)]
                                      (merge grid-ext (assoc grid' coord (cond
                                                                           (= v \.) (if (or (= 1 adj-bugs) (= 2 adj-bugs)) \# \.)
                                                                           (= v \#) (if (= 1 adj-bugs) \# \.))))))) {} grid))

(defn simulate-til-first-repeat
  [grid0]
  (let [grid (atom grid0)
        seen-grids (atom #{})]
    (do (while (not (contains? @seen-grids @grid))
          (do (swap! seen-grids #(conj % @grid))
              (swap! grid simulate))))
    (print-grid @grid)
    @grid))

(def level-grid (into {} (map (fn [[[x y] v]]
                                [[x y 0] v]) input)))

(defn simulate-til-end
  [grid0]
  (let [grid (atom grid0)
        i (atom 0)]
    (do (while (<= @i 11)
          (do (println "iteration" @i (count (filter (fn [[_ v]] (= v \#)) @grid)))
              (swap! grid simulate-2)
              (swap! i inc))))
    @grid))

(defn part1 [] (calc-biodiversity (simulate-til-first-repeat input)))

(defn part2 [] (count (filter (fn [[_ v]] (= v \#)) (simulate-til-end level-grid))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))