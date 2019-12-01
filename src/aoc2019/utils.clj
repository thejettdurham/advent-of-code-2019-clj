(ns aoc2019.utils
  (:require [clojure.java.io :as io] ))

(defn read-res-file
  "Reads file from resources as string"
  [file]
  (->> (io/resource file) slurp))