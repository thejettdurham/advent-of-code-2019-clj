(ns aoc2019.day12
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  (->> "day12.txt"
       utils/read-res-file
       clojure.string/split-lines
       (mapv (comp #(mapv read-string %) #(s/split % #" ") #(utils/strip % "<xyz=>")))))

(defn gravity
  [{p1 :pos v1 :vel} {p2 :pos v2 :vel}]
  (let [v1Ch (mapv (fn [i1 i2] (cond
                                 (= i1 i2) 0
                                 (> i1 i2) -1
                                 :else 1)) p1 p2)
        v1' (mapv + v1 v1Ch)]
    {:pos p1 :vel v1'}))

(defn do-gravity-step
  [moonsIn]
  (loop [iter 0
         moons moonsIn]
    (if (= iter 4) (reverse moons)
                   (let [focus (nth moons iter)
                         rest (utils/vec-remove moons iter)
                         moons' (reduce (fn [[f & rs] it]
                                                  (let [f' (gravity f it)]
                                                    (concat [f'] rs [it]))) [focus] rest)]
                     (recur (inc iter) (vec moons'))))))

(defn apply-vel
  [{p :pos v :vel}]
  {:pos (map + p v) :vel v})

(defn simulate
  [moons]
  (let [gravd (do-gravity-step moons)
        moons' (mapv apply-vel gravd)]
    moons'))

(defn simulate-until
  [input endStep]
  (loop [step 0
         moons (mapv (fn [m] {:pos m :vel [0 0 0]}) input)]
    (let [moons' (simulate moons)]
      (if (>= (inc step) endStep)
        moons'
        (recur (inc step) moons')))))

(defn seen-all-repeats
  [x]
  (= 3 (count (filter #(> % 0) x))))

(defn make-axis-comparer
  [x]
  (apply mapv vector (mapv (comp (fn [[p v]] (mapv vector p v)) (juxt :pos :vel)) x)))

(defn get-next-periods
  [step periods init moons]
  (let [curr (make-axis-comparer moons)]
    (map (fn [p i c] (if (not= p 0)
                       p
                       (if (= i c) step 0)))
         periods init curr)))

(defn simulate-until-same
  [input]
  (let [initMoons (mapv (fn [m] {:pos m :vel [0 0 0]}) input)
        init (make-axis-comparer initMoons)]
    (loop [step 0
          moons initMoons
          periods [0 0 0]]
     (let [moons' (simulate moons)
           periods' (get-next-periods step periods init moons)]
       (if (seen-all-repeats periods')
         (apply utils/lcmv (map int (flatten periods')))
         (recur (inc step) moons' periods'))))))

(defn calc-energy
  [moons]
  (let [sumAbs (fn [v] (->> v (map utils/abs) (reduce + 0)))]
    (reduce + 0 (map #(apply * %) (map
                         (juxt (comp sumAbs :pos) (comp sumAbs :vel))
                         moons)))))

(defn part1 [] (calc-energy (simulate-until input 1000)))

(defn part2 [] (simulate-until-same input))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))