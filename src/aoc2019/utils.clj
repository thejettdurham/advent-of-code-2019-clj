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