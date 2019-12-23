(ns aoc2019.utils
  (:require [clojure.java.io :as io]))

; Source - https://stackoverflow.com/a/18248031/11882366
(defn cartesian [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian (rest colls))
          x (first colls)]
      (cons x more))))

(defn read-res-file
  "Reads file from resources as string"
  [file]
  (->> (io/resource file) slurp))

(defn assoc-at [data i item]
  (if (associative? data)
    (assoc data i item)
    (if-not (neg? i)
      (letfn [(assoc-lazy [i data]
                (cond (zero? i) (cons item (rest data))
                      (empty? data) data
                      :else (lazy-seq (cons (first data)
                                            (assoc-lazy (dec i) (rest data))))))]
        (assoc-lazy i data))
      data)))

(defn interp-points
  "Generates an ordered seq of 1-points between x0 and x1"
  [x0 x1]
  (cond
    (== x0 x1) (cons x0 ())
    ; The inc/dec on the first term ensures we don't duplicate initCoord later
    (> x1 x0) (range (inc x0) (inc x1))
    :else (range (dec x0) (dec x1) -1)))

(def spy #(do (println "DEBUG:" %) %))

(defn print-px
  [px]
  (if (= px 0) "▓" "░"))

; Source https://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Clojure
(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

; Source https://stackoverflow.com/a/18319708/11882366
(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn abs
  [x]
  (Math/abs x))

; https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))
(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn get-adjacent-points
  [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

; https://rosettacode.org/wiki/Modular_exponentiation#Clojure
(defn modpow
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))