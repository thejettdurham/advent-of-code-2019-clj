(ns aoc2019.day16
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(def input
  (->> "day16.txt"
       utils/read-res-file
       (#(s/split %1 #""))
       (map read-string)))

; Given a list of nums, calc a new list of that same length
; Multiply each num in the list by a number from a pattern, then sum the results
; Shift the pattern, then do the same thing.
; The pattern and it's shifted variants is always the same between FFT cycles

; CALCULATING THE PATTERNS
; Base is [0 1 0 -1]
; When applying the pattern, skip the very first value exactly once.
; Pattern variant depends on the (inc idx) of the output list
; - Repeat each number in the pattern # times
; - Cycle the list
; - Discard the head
; - Take (count input)

(def base-pattern
  [0 1 0 -1])

(defn get-patterns
  "Get all of the xform patterns for a given input length"
  [input-len]
  (let [xs (range 0 input-len)]
    (map #(take input-len (rest (cycle (flatten (for [x base-pattern]
                                                 (repeat (inc %) x)))))) xs)))

(defn get-ones-digit
  [x]
  (read-string (str (last (str x)))))

(defn fft
  [input pattern]
  (get-ones-digit
    (reduce + 0
            (map * input pattern))))

(defn part1 []
  (s/join (take 8 (let [pats (get-patterns (count input))
                 cycles (range 0 100)]
             (reduce (fn [acc _]
                       (map #(fft acc %) pats)) input cycles)))))

; At least in Clojure, there's no way to realistically extend the part 1 approach to part 2 without running out of memory.
; So, you have to optimize the calculation. The key hint is that your message offset is very deep in the input,
; and by that point all of the modifiers in the first half of the list are 0, and are 1 otherwise, so the "FFT" becomes
; a simple sum once you consider only the input starting from your offset.
(defn part2 []
  (s/join (take 8 (let [size (* 10000 (count input))
                        msg-offset (read-string (s/join (take 7 input)))
                        real-input (take size (cycle input))
                        chop-input (drop msg-offset real-input)
                        cycles (range 0 100)]
                    (reduce (fn [out _]
                              (let [sum (atom (reduce + 0 out))]
                                (map (fn [val] (let [sum' @sum
                                                     nextSum (- sum' val)]
                                                 (do (reset! sum nextSum)
                                                     (if (>= sum' 0)
                                                       (mod sum' 10)
                                                       (mod (- sum') 10))))) out))) chop-input cycles)))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))