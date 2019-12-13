(ns aoc2019.intcode
  (:require [clojure.string :as s]
            [aoc2019.utils :as utils]))

(defn make-pMode->getter [relBase]
  {\0 nth
   \1 (fn [_ %2] (identity %2))
   \2 (fn [coll idx] (nth coll (+ relBase idx)))})

(defn is-relative-mode [m] (= m \2))

(defn- parse-cplx-op
  "Parse a complex op, yielding [opCode, x-getter, y-getter]"
  [op relBase]
  (let [[opCS pModes] (split-at 2 (s/reverse (str op)))
        [xMode yMode zMode] pModes
        [xGtr yGtr] (map #(get (make-pMode->getter relBase) %) pModes)
        opC (read-string (str "10r" (s/join (reverse opCS))))]
    ; default each getter to `nth`, as the pModes aren't always exhaustive
    [opC [(or xGtr nth) (is-relative-mode xMode)]
     [(or yGtr nth) (is-relative-mode yMode)]
     (is-relative-mode zMode)]))

(defn- calc-op-vals
  "Given current memory and instruction pointer,
  Calculates [opcode, next instruction pointer, x value, y value, and z register]"
  [mem ip relBase]
  (let [opCR (nth mem ip)]
    (if (= opCR 99)
      [99 0 [nil nil nil]]
      (let [rX (nth mem (+ 1 ip))
            rY (nth mem (+ 2 ip))
            rZ (nth mem (+ 3 ip))
            ; If this computer is reused again in a future task, then
            ; this way of checking the raw opcode may need to change
            [opC [xGtr xRel?] [yGtr yRel?] zRel?] (if (< opCR 10)
                                                     [opCR [nth false] [nth false] false]
                                                     (parse-cplx-op opCR relBase))
            xZ (if zRel? (+ rZ relBase) rZ)
            xX (if xRel? (+ rX relBase) rX)]
        ;(println xGtr rX)
        (cond
          (= opC 1) [1 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) xZ]]
          (= opC 2) [2 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) xZ]]
          (= opC 3) [3 (+ 2 ip) [nil nil xX]]
          (= opC 4) [4 (+ 2 ip) [(xGtr mem rX) nil nil]]
          (= opC 5) [5 (+ 3 ip) [(xGtr mem rX) (yGtr mem rY) nil]]
          (= opC 6) [6 (+ 3 ip) [(xGtr mem rX) (yGtr mem rY) nil]]
          (= opC 7) [7 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) xZ]]
          (= opC 8) [8 (+ 4 ip) [(xGtr mem rX) (yGtr mem rY) xZ]]
          (= opC 9) [9 (+ 2 ip) [(xGtr mem rX) nil nil]])))))

(defn run-cpu
  "Runs the cpu for a given input and starting value"
  ([code inputVals]
   "Simple case, doesn't support pausing, returns the last output when machine halts"
   (run-cpu 0 code 0 inputVals false))
  ([ip code relBase inputVals]
   "More robust case, allows the machine to be restarted later
   Supports pausing, yields collected outputs and [ip mem relBase] to be fed back in later"
   (run-cpu ip code relBase inputVals true))
  ([ip0 code relBase0 inputVals pause?]
   (loop [ip ip0
          ; Concat a never-ending seq of 0s for extended memory
          mem (concat code (repeat 0))
          output '()
          inputs inputVals
          relBase relBase0]
     (let [[opC nextIp [x y rZ]] (calc-op-vals mem ip relBase)
           shouldPause (and pause? (= opC 3) (nil? inputs))
           shouldHaltPause (and pause? (= opC 99))]
       ;(println opC rZ x y)
       (cond
         shouldHaltPause [(reverse output) nil]
         shouldPause [(reverse output) [ip mem relBase]]
         (= opC 99) (reverse output)
         (= opC 1) (recur nextIp (utils/assoc-at mem rZ (+ x y)) output inputs relBase)
         (= opC 2) (recur nextIp (utils/assoc-at mem rZ (* x y)) output inputs relBase)
         (= opC 3) (recur nextIp (utils/assoc-at mem rZ (first inputs)) output (next inputs) relBase)
         (= opC 4) (recur nextIp mem (conj output x) inputs relBase)
         (= opC 5) (recur (if (not= 0 x) y nextIp) mem output inputs relBase)
         (= opC 6) (recur (if (= 0 x) y nextIp) mem output inputs relBase)
         (= opC 7) (recur nextIp
                          (utils/assoc-at mem rZ (if (< x y) 1 0))
                          output
                          inputs
                          relBase)
         (= opC 8) (recur nextIp (utils/assoc-at mem rZ (if (= x y) 1 0)) output inputs relBase)
         (= opC 9) (recur nextIp mem output inputs (+ relBase x)))))))