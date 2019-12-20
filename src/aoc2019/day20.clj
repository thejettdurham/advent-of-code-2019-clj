(ns aoc2019.day20
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

(defn is-lbl [x]
  (and (>= (int x) 65) (<= (int x) 90)))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-input
  [rows]
  (let [cols (transpose (map #(take 130 (concat % (repeat \space))) rows))
        grid (reduce-kv (fn [acc yi y]
                          (reduce-kv (fn [acc2 xi x]
                                       (assoc acc2 [xi yi] x)) acc y)) {} rows)
        init (atom [])
        exit (atom [])
        label-portal (atom [])
        portals (atom {})
        scan-col (fn [x rev? inner?]
                   (let [col (nth cols x)
                         dx (if rev? dec inc)]
                     (dorun (keep-indexed (fn [y c] (if (is-lbl c)
                                           (let [s1 c
                                                 s2 (grid [(dx x) y])
                                                 lbl (if rev? (str s2 s1) (str s1 s2))
                                                 dm (if inner? inc dec) ; depth modifier for p2
                                                 crd [(dx (dx x)) y dm]]
                                             (swap! label-portal #(conj % [lbl crd]))))) col))))
        scan-row (fn [y rev? inner?]
                   (let [row (nth rows y)
                         dy (if rev? dec inc)]
                     (dorun (keep-indexed (fn [x c] (if (is-lbl c)
                                           (let [s1 c
                                                 s2 (grid [x (dy y)])
                                                 lbl (if rev? (str s2 s1) (str s1 s2))
                                                 dm (if inner? inc dec)
                                                 crd [x (dy (dy y)) dm]]
                                             ; Looks like ["TV" [2 41 dec]] for my input
                                             (swap! label-portal #(conj % [lbl crd]))))) row))))]
    ; Scan the key row/cols where the labels start
    (scan-row 0 false true)
    (scan-row 38 true false)
    (scan-row 90 false false)
    (scan-row 128 true true)
    (scan-col 0 false true)
    (scan-col 38 true false)
    (scan-col 88 false false)
    (scan-col 126 true true)
    ; map label to coords
    (let [lbl->coords (reduce (fn [acc [lbl coord]]
                                (cond
                                  (= lbl "AA") (do (reset! init coord) acc)
                                  (= lbl "ZZ") (do (reset! exit coord) acc)
                                  :else (if (contains? acc lbl) (update acc lbl #(conj % coord))
                                                                (conj acc {lbl [coord]})))) {} @label-portal)]
      (dorun (map (fn [[_ [[x0 y0 :as a] [x1 y1 :as b]]]]
                    (swap! portals #(into % {[x0 y0] b [x1 y1] a}))) lbl->coords)))
    [grid @portals @init @exit]))

(def input
  (->> "day20.txt"
       utils/read-res-file
       clojure.string/split-lines
       (#(mapv (fn [y] (mapv identity y)) %))))

(def is-open #(= % \.))

(defn get-adj
  [portals coord]
  (let [adj-coords (utils/get-adjacent-points coord)
        [x y :as portal-coord] (portals coord)]
    (if (nil? portal-coord)
      adj-coords
      (conj adj-coords [x y]))))

(defn get-adj-2
  [portals coord]
  (let [adj-coords (map #(conj % identity) (utils/get-adjacent-points coord))
        portal-coord (portals coord)]
    (if (nil? portal-coord)
      adj-coords
      (conj adj-coords portal-coord))))

(defn walk
  [grid portals [x0 y0] [x1 y1]]
  (let [seen-pts (atom #{})
        init [x0 y0]
        exit [x1 y1]
        queue (atom [[init (grid init) 0]])
        count-steps (atom 0)]
    (do (while (and (= 0 @count-steps) (> (count @queue) 0))
          (do
            (let [[coord val steps] (first @queue)]
              (println coord val steps)
              (swap! queue rest)
              (if (contains? @seen-pts coord)
                nil
                 (do (swap! seen-pts #(conj % coord))
                     (if (= 0 (mod (count @seen-pts) 10000)) (println (count @seen-pts)))
                     (cond
                       (= coord exit) (reset! count-steps steps)
                       (is-open val) (let [np (get-adj portals coord)
                                           ns (map (fn [x] [x (grid x) (inc steps)]) np)]
                               (swap! queue #(concat % ns)))
                       :else nil))))
            )) @count-steps)))

(defn walk-2
  [grid portals [x0 y0] [x1 y1]]
  (let [seen-pts (atom #{})
        init [x0 y0]
        exit [x1 y1]
        queue (atom [[init (grid init) 0 0]])
        count-steps (atom 0)]
    (do (while (and (= 0 @count-steps) (> (count @queue) 0))
          (do
            (let [[coord val steps depth] (first @queue)]
              ;(println coord val steps depth)
              (swap! queue rest)
              (if (or (neg? depth) (contains? @seen-pts [coord depth]))
                nil
                (do (swap! seen-pts #(conj % [coord depth]))
                    (if (= 0 (mod (count @seen-pts) 10000)) (println (count @seen-pts)))
                    (cond
                      (and (= depth 0) (= coord exit)) (reset! count-steps steps)
                      (is-open val) (let [np (get-adj-2 portals coord)
                                          ns (map (fn [[x y dm]] [[x y] (grid [x y]) (inc steps) (dm depth)]) np)]
                                      (swap! queue #(concat % ns)))
                      :else nil))))
            )) @count-steps)))

(defn part1 [] (apply walk (parse-input input)))

(defn part2 [] (apply walk-2 (parse-input input)))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))