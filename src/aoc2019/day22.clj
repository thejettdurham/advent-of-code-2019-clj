(ns aoc2019.day22
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math]))

(defn cut-n
  [n]
  (fn [ncards i] (let [shift (- i n)]
                   (if (>= shift 0) shift (+ ncards shift)))))

(defn deal-with-increment-n
  [n]
  (fn [ncards i]
    (if (= 0 i) 0
                (mod (* i n) ncards))))

(defn deal-into-new-stack-n
  []
  (fn [ncards i]
    (- (dec ncards) i)))

(defn parse-instr-1
  [l]
  (cond
    (s/starts-with? l "cut") (cut-n (read-string (second (s/split l #" "))))
    (s/starts-with? l "deal with increment") (deal-with-increment-n (read-string (nth (s/split l #" ") 3)))
    :else (deal-into-new-stack-n)))

(defn parse-instr-2
  [l]
  (cond
    (s/starts-with? l "cut") ["cut" (read-string (second (s/split l #" ")))]
    (s/starts-with? l "deal with increment") ["inc" (read-string (nth (s/split l #" ") 3))]
    :else ["stack" nil]))

(def input
  (->> "day22.txt"
       utils/read-res-file
       clojure.string/split-lines))

(defn shuff [instr ncards i]
  (reduce (fn [acc f]
            (f ncards acc)) i instr))


(defn part1 [] (shuff (map parse-instr-1 input) 10007 2019))


; A crappy implementation of Sophie Alpert's algorithm.
; https://github.com/sophiebits/adventofcode/blob/master/2019/day22.py
; The key idea is to work backwards...the numbers are too large that it's infeasible to run forwards
; Modular inverse is the key to this, but this is knowledge I don't have and at this point don't expect to ever need
; I don't fully understand it, but it works
(defn part2 [] (let [a (atom 1)
                     b (atom 0)
                     ld 119315717514047
                     times 101741582076661
                     card 2020]
                 (dorun (map
                          (fn [l]
                               (let [[action x] (parse-instr-2 l)]
                                 (cond
                                   (= action "inc") (do
                                                      (swap! a * (utils/modpow x (- ld 2) ld))
                                                      (swap! b * (utils/modpow x (- ld 2) ld)))
                                   (= action "cut") (do (swap! b + x))
                                   (= action "stack") (do (swap! a * -1)
                                                          (swap! b #(* -1 (inc %)))))
                                 (swap! a #(mod % ld))
                                 (swap! b #(mod % ld)))) (reverse input)))
                        (let [foo (utils/modpow @a times ld)
                              aa (* foo card)
                              bb (* @b (+ (dec ld) foo) (utils/modpow (dec @a) (- ld 2) ld))]
                          (str (mod (+ aa bb) ld)))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))