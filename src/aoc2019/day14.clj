(ns aoc2019.day14
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  (->> "day14.txt"
       utils/read-res-file
       s/split-lines))

(defn make-product-map
  [in]
  (map (comp
         reverse
         (fn [[[in] [out]]] [((comp
                                #(map (comp
                                        vec
                                        reverse
                                        (fn [[amt prd]] [(read-string amt) prd])
                                        (fn [x] (s/split x #" "))) %)
                                #(s/split % #", ")) in)
                             ((comp
                                reverse
                                (fn [[amt prd]] [(read-string amt) prd])
                                #(s/split % #" ")) out)])
         #(split-at 1 %)
         #(s/split % #" => ")) in))

(defn ore-for-fuel
  [prods inventory]
  (letfn [(produce [name amt]
         (let [ore (atom 0)
               [[outP out#] ins] (first (filter #(= name (first (first %))) prods))
               mult (bigint (Math/ceil (/ amt out#)))]
           (dorun (map (fn [[inP in#]]
                         (if (= inP "ORE")
                           (swap! ore + (* mult in#))
                           (do
                             (if (not (contains? @inventory inP))
                               (swap! inventory #(assoc % inP 0)))

                             (if (< (get @inventory inP) (* mult in#))
                               (swap! ore #(+ % (produce inP (- (* mult in#) (get @inventory inP))))))

                             (swap! inventory #(assoc % inP (- (get % inP) (* mult in#))))))) ins))

           (if (not (contains? @inventory outP))
             (swap! inventory #(assoc % outP 0)))
           (swap! inventory #(assoc % outP (+ (get % outP) (* mult out#))))
           @ore))]
    produce))

(defn part1 [] (let [prods (make-product-map input)
                     inventory (atom {})]
                 ((ore-for-fuel prods inventory) "FUEL" 1)))

; Boy, this is awful
(defn part2 []
  (let [prods (make-product-map input)
        limit 1000000000000
        lastMake (atom 0)
        make (atom 100)
        lastStep (atom 10)
        step (atom 10)
        steps (atom 0)
        reset-steps #(reset! steps 0)
        this (atom 0)
        last (atom 0)
        doIt (partial (ore-for-fuel prods (atom {})) "FUEL")]
    (do
      (while (not (and (= @lastStep 1) (< @last limit) (>= @this limit)))
        (do
          (reset! lastStep @step)
          (reset! last @this)
          (reset! this (doIt @make))
          (swap! steps inc)
          (cond
            (and (<= @last limit) (> @this limit)) (do (reset-steps) (swap! step #(max 1 (quot % 10))))
            (> @steps 10) (do (reset-steps) (swap! step * 10)))
          (reset! lastMake @make)
          (swap! make (if (< @this limit) + -) @step)))
      @make)))


(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))