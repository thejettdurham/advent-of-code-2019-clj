(ns aoc2019.day13
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [aoc2019.intcode :as intcode]
            [lanterna.terminal :as t]))

(def tile-map
  {0 \space
   1 \▓
   2 \░
   3 \╤
   4 \○ })

(def input-map
  {:left  -1
   :right 1})

(def term (t/get-terminal :swing))
(def put-character-to-term (partial t/put-character term))
(def move-cursor (partial t/move-cursor term))
(def write #(dorun (map put-character-to-term %)))

(t/start term)

(defn write-pxs
  [pxs]
  (dorun (map (fn [[x y tI]]
                (if (= x -1)
                  (do (move-cursor 0 1)
                      (write (str "Score: " tI)))
                  (let [t (tile-map tI)]
                    (if (not (nil? t)) (do (move-cursor x (+ 2 y))
                                           (put-character-to-term (tile-map tI)))))))
              pxs)))

(defn read-input
  [in]
  (->> in
       utils/read-res-file
       (#(s/split %1 #","))
       (map read-string)))

(def input (read-input "day13.txt"))

; This input has a bottom wall added, so the ball will bounce around forever until all blocks are hit
; The map is stored sequentially in the program, so it's pretty easy to find the bit to edit.
(def input-cheat (read-input "day13ch.txt"))

(defn play-game-interactive
  [[_ & input]]
  (try (let [in (conj input 2)
         [pgm state] (intcode/run-cpu 0 in 0 [0])
         screen (partition 3 pgm)]
     (write-pxs screen)
     (move-cursor 0 0)
     (loop [[ip mem relBase] state]
       (let [rawIn (t/get-key-blocking term)
             joyIn (get input-map rawIn 0)
             [pgm' state'] (intcode/run-cpu ip mem relBase [joyIn])
             screen' (partition 3 pgm')]
         (write-pxs screen')
         (move-cursor 0 0)
         (recur state'))))
       (catch Exception e (println "D'oh!"))))

(defn play-game-cheat
  [[_ & input]]
  (try (let [in (conj input 2)
             [pgm state] (intcode/run-cpu 0 in 0 [0])
             screen (partition 3 pgm)]
         (write-pxs screen)
         (move-cursor 0 0)
         (loop [[ip mem relBase] state]
           (let [[pgm' state'] (intcode/run-cpu ip mem relBase [0])
                 screen' (partition 3 pgm')]
             (write-pxs screen')
             (move-cursor 0 0)
             (recur state'))))
       (catch Exception e (println "D'oh!"))))

(defn part1 [] (count (filter #(= 2 %) (map #(nth % 2) (partition 3 (intcode/run-cpu input []))))))

(defn part2 [] (play-game-cheat input-cheat))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: (check the popout terminal)" (part2)))))