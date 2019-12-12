(ns aoc2019.day10
  (:require [aoc2019.utils :as utils]
            [clojure.string :as s]
            [clojure.set :as set]))

; Confession: I had no idea where to start with this problem. I understood what was being asked
; and understood the problem constraints, but this kind of problem is way out of my wheelhouse.
; I spent about 3 hours working a solution that tried to trace a line from each asteroid to each
; perimeter asteroid, but was really not getting anywhere with it
; Even if I'd had the hint of "use polar coordinates", it's been over 15 years since I've worked
; worked with them in an academic setting, and I've never worked with them in a code setting.
; So, I decided to invert the exercise and do a detailed analysis of a working solution.
; This helped me fill some gaps in my clojure knowledge and, upon picking it apart, I understand
; now what the key insights were and how to solve problems like this in the future.

; For reference: https://github.com/armstnp/advent-of-code-2019/blob/master/src/advent_of_code_2019/day10.clj

(def input
  (->> "day10.txt"
       utils/read-res-file
       clojure.string/split-lines
       ; Like normal map but emits a vector instead of a list
       (mapv vec)))

(defn width
  [input]
  (count (first input)))

(defn height
  [input]
  (count input))

; All asteroid points
(defn map->asteroids
  [input]
  ; 📝 I need to get more comfortable with list comprehensions
  (for [x (range 0 (width input))
        y (range 0 (height input))
        ; 🧠 Use the input as a function rather than using nth
        :when (= \# ((input y) x))]
    [x y]))

(defn polar
  ; 🧠 use :as in destructuring to have a ref to the structured element
  [source target]
  ; 🧠 map can do pair-wise operation on seqs (operating like zipWith from haskell)
  ; 🧠 Maps are really easy to use, do it instead of vecs
  ; :r is the radius of the polar line
  ;   - Subtract target pts from source pts, take abs, and sum them
  ; :theta is the angle of the polar line
  ;   - Subtract the source pts from target pts,
  ;   - take vals as doubles (^:double is a type hint),
  ;   - take atan2, subtract from pi
  {:r (apply + (map (comp #(Math/abs %) -) source target)) ;; Close enough for our purposes
   :theta (- Math/PI (apply #(Math/atan2 %1 %2) ^:double (map (comp double -) target source)))
   :asteroid target})


; 🔑️ Blocked asteroids have the same theta as another asteroid
(defn best-candidate
  [input]
  (let [asteroids (map->asteroids input)
        ; Calculate polar lines from each asteroid to each other non-self asteroid
        asteroid-lines (for [station asteroids
                             target asteroids
                             :when (not= station target)]
                         ; vecs of station and theta to each other asteroid
                         [station (:theta (polar station target))])
        _ (println (count asteroid-lines))]
    (->> asteroid-lines
         ; Remove all duplicate lines, only interested in first asteroid hit per line
         distinct
         ; Collect the stations
         (map first)
         ; Get freqs
         frequencies
         ; val is an arg to max-key, not apply
         ; max-key interprets map as entry tuple
         (apply max-key val))))

; 🔑 You don't need to sweep around arbitrarily, you can jump from asteroid to asteroid if you know
; what order to go in. This bit puts those asteroids in that order, grouped by their angle from
; the base station, and colinear asteroids sorted from smallest radius to largest
(defn polar-lines
  [input station]
  (let [asteroids (map->asteroids input)
        ; polar coords to each asteroid from station
        polars (for [target asteroids
                     :when (not= station target)]
                 (polar station target))]
    (->> polars
         ; Group by similar theta, sort asc
         (group-by :theta)
         (sort-by first)
         ; map over each group
         ; Take the listZxZX ascending (shortest line to longest line)
         ; get the asteroid for that coordinate
         (map (comp (partial map :asteroid) (partial sort-by :r) second)))))

; Loop through the sorted asteroid lines, removing the head of each line, skipping empty lines
; The current line is removed from the top of the list and goes to the end.
; Continue this process until n is 1, in which case return the next asteroid
(defn laser-until
  ; 🧠 Like doing x:xs in haskell...get the head and rest
  [[[next-asteroid & rest-asteroids] & rest-lines] n]
  (if (= n 1)
    next-asteroid
    (let
      ; If there are no more asteroids in this line, make line' empty so it will disappear
      [line' (if (empty? rest-asteroids) [] [rest-asteroids])
      ; Put line' at the end of the list
      lines' (concat rest-lines line')]
      (recur lines' (dec n)))))

(defn -main
  []
  (do (println (str "Part 1: " (second (best-candidate input))))
      (println (str "Part 2: " (let [station (first (best-candidate input))
                                     [x y] (laser-until (polar-lines input station) 200)]
                                 (+ y (* 100 x))
                                )))))