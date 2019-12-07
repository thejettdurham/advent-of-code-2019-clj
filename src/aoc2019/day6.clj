(ns aoc2019.day6
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]))

; Source https://gist.github.com/stathissideris/1397681b9c63f09c6992
(defn tree-seq-depth
  "Returns a lazy sequence of vectors of the nodes in a tree and their
  depth as [node depth], via a depth-first walk.  branch? must be a fn
  of one arg that returns true if passed a node that can have
  children (but may not).  children must be a fn of one arg that
  returns a sequence of the children. Will only be called on nodes for
  which branch? returns true. Root is the root node of the tree."
  [branch? children root]
  (let [walk (fn walk [depth node]
               (lazy-seq
                 (cons [node depth]
                       (when (branch? node)
                         (mapcat (partial walk (inc depth)) (children node))))))]
    (walk 0 root)))
(defn tree-seq-path
  "Like core's tree-seq but returns a lazy sequence of vectors of the
  paths of the nodes in a tree, via a depth-first walk. It optionally
  applies node-fn to each node before adding it to the path. branch?
  must be a fn of one arg that returns true if passed a node that can
  have children (but may not).  children must be a fn of one arg that
  returns a sequence of the children. Will only be called on nodes for
  which branch? returns true. Root is the root node of the tree."
  [branch? children root & [node-fn]]
  (let [node-fn (or node-fn identity)
        walk (fn walk [path node]
               (let [new-path (conj path (node-fn node))]
                 (lazy-seq
                   (cons new-path
                         (when (branch? node)
                           (mapcat (partial walk new-path) (children node)))))))]
    (walk [] root)))

(def input
  "Parsed to a map: orbitee->[orbiter]"
  (->> "day6.txt"
       utils/read-res-file
       clojure.string/split-lines
       (map (comp #(s/split % #"_") #(s/replace % \) \_)))
       (reduce
         (fn [m [k v]]
           (if (contains? m k)
             (update m k #(cons v %))
             (assoc m k (list v)))) {})))

(defn count-orbits [tree]
  (reduce (fn [acc [_ d]] (+ acc d)) 0 (tree-seq-depth next rest tree)))

(defn get-node [input node]
  (let [val (get input node)]
    (if (nil? val)
      (list node)
      (cons node (map #(get-node input %) val)))))

(defn is-ys-leaf
  [x]
  (or (= x '("SAN")) (= x '("YOU"))))

(defn parse-tree [in] (get-node in "COM"))

(defn node-contains-both
  [x]
  (and (contains? (set x) "YOU") (contains? (set x) "SAN")))

(defn find-min-dist
  "Given the set of YOU and SAN paths,
  Find the minimum distance to move YOU to SAN"
  [x y]
  (loop [xIdx nil
         yIdx nil
         i 0]
    (if (and (some? xIdx) (some? yIdx))
      ; Dec each index, because they end at the same node
      (+ (dec xIdx) (dec yIdx))
      (let [xv (nth x i)
            yv (nth y i)
            xI (if (node-contains-both xv) i nil)
            yI (if (node-contains-both yv) i nil)]
        (recur (first (filter some? [xIdx xI]))
               (first (filter some? [yIdx yI]))
               (inc i))
        ))))

(defn find-you-san-paths
  "Finds bottom-up vecs of paths to YOU and SAN"
  [in]
  (map #(map flatten (reverse %))
       (filter #(is-ys-leaf (last %))
               (tree-seq-path next rest in))))

(defn part1 [] (count-orbits (parse-tree input)))

(defn part2 [] (apply find-min-dist (find-you-san-paths (parse-tree input))))

(defn -main
  []
  (do (println (str "Part 1: " (part1)))
      (println (str "Part 2: " (part2)))))