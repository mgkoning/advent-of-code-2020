(ns day12.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day12.txt")))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-instruction [i]
  [(get i 0) (parse-int (subs i 1))])

(defn add-vector [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn apply-pos-instruction-part1 [[command arg] pos [dx dy :as dir]]
  (cond
    (= \N command) (add-vector pos [0 (- arg)])
    (= \S command) (add-vector pos [0 arg])
    (= \W command) (add-vector pos [(- arg) 0])
    (= \E command) (add-vector pos [arg 0])
    (= \F command) (add-vector pos [(* arg dx) (* arg dy)])
    :else pos))

(defn left 
  "Rotate a vector left (i.e., -90 degrees)"
  [[dx dy]]
  [dy (- dx)])

(defn right
  "Rotate a vector right (i.e., 90 degrees)"
  [[dx dy]]
  [(- dy) dx])

(defn apply-dir-instruction [[command arg] [dx dy :as dir]]
  (let [turns (quot arg 90)]
    (cond
      (= \L command) (nth (iterate left dir) turns)
      (= \R command) (nth (iterate right dir) turns)
      :else dir)))

(defn follow-instruction-part1 [{ pos :pos dir :dir} instruction]
  {:pos (apply-pos-instruction-part1 instruction pos dir)
   :dir (apply-dir-instruction instruction dir)})

(defn follow-instructions-part1 [instructions]
  (reduce follow-instruction-part1 {:pos [0 0] :dir [1 0]} instructions))

(defn apply-pos-instruction-part2 [[command arg] pos [dx dy]]
  (if
    (= \F command) (add-vector pos [(* arg dx) (* arg dy)])
    pos))

(defn apply-waypoint-instruction [[command arg] waypoint]
  (let [turns (quot arg 90)]
    (cond
      (= \N command) (add-vector waypoint [0 (- arg)])
      (= \S command) (add-vector waypoint [0 arg])
      (= \W command) (add-vector waypoint [(- arg) 0])
      (= \E command) (add-vector waypoint [arg 0])
      (= \L command) (nth (iterate left waypoint) turns)
      (= \R command) (nth (iterate right waypoint) turns)
      :else waypoint)))

(defn follow-instruction-part2 [{pos :pos waypoint :waypoint} instruction]
  {:pos (apply-pos-instruction-part2 instruction pos waypoint) 
   :waypoint (apply-waypoint-instruction instruction waypoint)})

(defn follow-instructions-part2 [instructions]
  (reduce follow-instruction-part2 {:pos [0 0] :waypoint [10 -1]} instructions))


(def instructions (map parse-instruction (get-input-lines)))

(defn abs [a] (if (< a 0) (- a) a))

(defn -main
  [& args]
  (let [{[x y] :pos dir :dir} (follow-instructions-part1 instructions)]
    (println "Part 1:")
    (println (+ (abs x) (abs y)))
    (let [{[x y] :pos} (follow-instructions-part2 instructions)]
      (println "Part 2:")
      (println (+ (abs x) (abs y))))))

