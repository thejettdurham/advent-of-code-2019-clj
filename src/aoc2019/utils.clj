(ns aoc2019.utils
  (:require [clojure.java.io :as io] ))

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