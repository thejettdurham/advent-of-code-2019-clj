(ns aoc2019.day18
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [clojure.set :as set]))

(def is-wall (partial = \#))
(defn is-key [x]
  (and (>= (int x) 97) (<= (int x) 122)))
(defn is-door [x]
  (and (>= (int x) 65) (<= (int x) 90)))

(defn parse-input
  [in]
  (let [init (atom [])
        grid (atom {})]
    (dorun (keep-indexed (fn [yi y]
                           (dorun (keep-indexed (fn [xi x]
                                            (do (swap! grid #(assoc % [xi yi] x))
                                                (if (= x \@) (reset! init [xi yi])))) y))) in))
    [@grid @init]))

(defn parse-input-2
  [in]
  (let [inits (atom [])
        grid (atom {})
        all-keys (atom {})]
    (dorun (keep-indexed (fn [yi y]
                           (dorun (keep-indexed (fn [xi x]
                                                  (do (swap! grid #(assoc % [xi yi] x))
                                                      (cond
                                                        (= x \@)(swap! inits #(conj % [xi yi]))
                                                        (is-key x) (swap! all-keys #(conj % [xi yi]))
                                                        ))) y))) in))
    [@grid @inits @all-keys]))

(def input
  (->> "day18.txt"
       utils/read-res-file
       clojure.string/split-lines
       parse-input))

(def input-2
  (->> "day18p2.txt"
       utils/read-res-file
       clojure.string/split-lines
       parse-input-2))

(defn walk-to-key
  [grid start]
  (let [seen-pts (atom #{})
        queue (atom [[start (grid start) #{} 0]])
        count-steps (atom 0)]
    (do (while (and (= 0 @count-steps) (> (count @queue) 0))
          (do
            (let [[coord val keys steps] (first @queue)]
                (swap! queue rest)
                (if (contains? @seen-pts [coord keys]) nil
                  (do (swap! seen-pts #(conj % [coord keys]))
                      (if (= 0 (mod (count @seen-pts) 10000)) (println (count @seen-pts)))
                      (cond
                        (nil? val) nil
                        (is-wall val) nil
                        (and (is-door val) (not (contains? keys (char (+ 32 (int val)))))) nil
                        :else (let [nextKeys (if (is-key val) (conj keys val) keys)
                                    np (utils/get-adjacent-points coord)
                                    ns (map (fn [x] [x (grid x) nextKeys (inc steps)]) np)]
                                (if (= 26 (count nextKeys))
                                  (do (println "got all keys in " steps) (reset! count-steps steps)))
                                (swap! queue #(concat % ns))
                                )))))
              )) @count-steps)))

(defn walk-to-key-2
  [grid starts all-keys]
  (let [best (atom 1000000000)
        seen-pts (atom #{})
        queue (atom [[starts #{} 0]])
        count-steps (atom 0)]
    (do (while (and (= 0 @count-steps) (> (count @queue) 0))
          (do
            (let [[coords keys steps] (first @queue)]
              (swap! queue rest)
              (if
                (contains? @seen-pts [coords keys])
                nil
                (do (swap! seen-pts #(conj % [coords keys]))
                    (if (= 0 (mod (count @seen-pts) 10)) (println (count @seen-pts)))
                    (let [bad-state (atom false)]
                      (dorun (map (fn [c]
                                    (let [val (grid c)]
                                      (cond
                                        (nil? val) (reset! bad-state true)
                                        (is-wall val) (reset! bad-state true)
                                        (and (is-door val) (not (contains? keys (char (+ 32 (int val)))))) (reset! bad-state true))))
                                  coords))
                      (if (false? @bad-state)
                        (let [d (atom {})
                              q2 (atom [(keep-indexed (fn [i x] x i 0) coords)])]
                          (while (> (count @q2) 0)
                            (let [[coord bot steps'] (first @q2)
                                  val (grid coord)]
                              (cond
                                (contains? @d coord) nil
                                (nil? val) nil
                                (is-wall val) nil
                                (and (is-door val) (not (contains? keys (char (+ 32 (int val)))))) nil
                                :else (do
                                        (swap! d #(assoc % coord [steps' bot]))
                                        (let [np (utils/get-adjacent-points coord)
                                              ns (map (fn [x] [x bot (inc steps)]) np)]
                                          (swap! q2 #(concat % ns)))))))
                          (dorun (map (fn [[key kcoord]]
                                        (if (and (not (contains? keys key))
                                                 (contains? d kcoord))
                                          (let [[d r] (get @d kcoord)
                                                newpos (map (fn [] kcoord) coords)
                                                newkeys (conj keys key)
                                                newdist (+ steps d)]
                                            (if (= 26 (count newkeys))
                                              (if (< newdist best)
                                                (do (reset! best newdist) (println "BEST!!" best))))
                                            (swap! queue #(conj % [newpos newkeys newdist]))))) all-keys))

                          ))))))
            )) @best)))

(defn part1 [] (let [[grid init] input]
                 (walk-to-key grid init)))

; NOTE: This still isn't working yet. I know what the answer is to be for my input, but I'm still working
; on implementing the algorithm here 
(defn part2 [] (let [[grid inits all-keys] input-2]
                 (walk-to-key-2 grid inits all-keys)))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))