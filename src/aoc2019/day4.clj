(ns aoc2019.day4
  (:require [aoc2019.utils :as utils]))

(def input
  (->> "day4.txt"
       utils/read-res-file
       (#(clojure.string/split %1 #"-"))
       (map read-string)))

(defn get-rng [x y] (range x (inc y)))
(def pwd-rng (apply get-rng input))

(defn explode-num [x]
  (map read-string (clojure.string/split (str x) #"")))

(defn combine-adjacent [input]
  (->> input (partition-by identity) (map first)))

(defn has-adjacent [x]
  (not= (count x) (count (combine-adjacent x))))

(defn has-adjacent-2 [x]
  (not (empty? (filter #(= 2 (count %)) (partition-by identity x)))))

(defn is-asc [x]
  (= x (sort x)))

(defn valid-pwd1?
  [x] ((every-pred is-asc has-adjacent) (explode-num x)))

(defn valid-pwd2?
  [x] ((every-pred is-asc has-adjacent-2) (explode-num x)))

(defn part1 [] (count (filter valid-pwd1? pwd-rng)))

(defn part2 [] (count (filter valid-pwd2? pwd-rng)))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))