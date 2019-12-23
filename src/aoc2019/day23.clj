(ns aoc2019.day23
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]))

(def input
  (->> "day23.txt"
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(def cpus (into {} (map #(vector % [0 input 0]) (range 0 50))))



(defn part1 [] (let [queues (atom (into {} (map #(vector % [] ) (range 0 50))))
                     nat (atom [])
                     handle-outputs (fn [out] (let [outs (partition 3 out)]
                                                (dorun (map (fn [[d & p]]
                                                              (if (= d 255) (reset! nat p))
                                                              (swap! queues (fn [q] (update q d #(conj % (vec p)))))) outs))))
                     cursor (atom 0)
                     machines (atom (reduce-kv (fn [acc k [a b c]]
                                                 (let [[out s] (intcode/run-cpu a b c [k -1])]
                                                   (handle-outputs out)
                                                   (assoc acc k s))) {} cpus))
                     run-mach-i (fn [i] (let [s (get @machines i)
                                              ins? (get @queues i)
                                              ins (if (empty? ins?) [[-1]] ins? )
                                              [out s] (reduce (fn [[out [a b c]] p]
                                                                (let [[out' s'] (intcode/run-cpu a b c p)]
                                                                  [(concat out out') s'])) [[] s] ins)]
                                          (handle-outputs out)
                                          (swap! machines #(assoc % i s))
                                          (swap! queues #(assoc % i []))))]
                 (do (while (empty? @nat)
                       (do (run-mach-i @cursor)
                           (swap! cursor #(mod (inc %) 50)))))

                 (second @nat)))

(defn part2 [] (let [queues0 (into {} (map #(vector % [] ) (range 0 50)))
                     queues (atom queues0)
                     nat (atom [])
                     handle-outputs (fn [out] (let [outs (partition 3 out)]
                                                (dorun (map (fn [[d & p]]
                                                              (if (= d 255) (reset! nat p)
                                                                            (swap! queues (fn [q] (update q d #(conj % (vec p))))))) outs))))
                     cursor (atom 0)
                     machines (atom (reduce-kv (fn [acc k [a b c]]
                                                 (let [[out s] (intcode/run-cpu a b c [k -1])]
                                                   (handle-outputs out)
                                                   (assoc acc k s))) {} cpus))
                     run-mach-i (fn [i] (let [s (get @machines i)
                                              ins? (get @queues i)
                                              ins (if (empty? ins?) [[-1]] ins? )
                                              ;_ (println "df" ins? ins)
                                              [out s] (reduce (fn [[out [a b c]] p]
                                                                (let [[out' s'] (intcode/run-cpu a b c p)]
                                                                  [(concat out out') s'])) [[] s] ins)]
                                          (handle-outputs out)
                                          (swap! machines #(assoc % i s))
                                          (swap! queues #(assoc % i []))))
                     net-idle? (fn [] (= @queues queues0))
                     last-y (atom nil)
                     first-twice-y (atom nil)]

                 (do (while (nil? @first-twice-y)
                       (if (net-idle?)
                         (if (= @last-y (second @nat))
                           (reset! first-twice-y @last-y)
                           (do (println "resetting with nat" @nat)
                               (swap! queues #(assoc % 0 [@nat]))
                               (reset! last-y (second @nat))
                               (reset! cursor 0)))
                         (do
                           ;(println @cursor (get @queues @cursor))
                             (run-mach-i @cursor)
                             (swap! cursor #(mod (inc %) 50))))))
                 @first-twice-y))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))