(ns aoc2019.day8
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  (->> "day8.txt"
       utils/read-res-file
       (#(s/split %1 #""))
       (map read-string)
       (partition (* 25 6))))

(defn part1 [] (apply * (vals (dissoc (first
                                        (sort-by #(get % 0) #(compare %1 %2)
                                                 (map frequencies input))) 0))))

(defn resolve-pixel
  [bot top]
  (if (= 2 top) bot top))

(defn resolve-layers
  [in]
  (reduce (fn [acc nextLayer]
            (map resolve-pixel acc nextLayer)) (first in) (next in)))

(defn part2 [] (s/join "\n" (map s/join
                     (partition 25 (map utils/print-px (resolve-layers (reverse input)))))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: \n" (part2)))))