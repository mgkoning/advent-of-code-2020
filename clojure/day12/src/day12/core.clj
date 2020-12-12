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

(def east-dir  [1  0 ])
(def west-dir  [-1 0 ])
(def north-dir [0  -1])
(def south-dir [0  1 ])

(defn left-part1 [dir]
  (cond
    (= west-dir dir) south-dir
    (= south-dir dir) east-dir
    (= east-dir dir) north-dir
    (= north-dir dir) west-dir))

(defn right-part1 [dir]
  (cond
    (= west-dir dir) north-dir
    (= north-dir dir) east-dir
    (= east-dir dir) south-dir
    (= south-dir dir) west-dir))


(defn apply-dir-instruction-part1 [[command arg] [dx dy :as dir]]
  (let [turns (quot arg 90)]
    (cond
      (= \L command) (nth (iterate left-part1 dir) turns)
      (= \R command) (nth (iterate right-part1 dir) turns)
      :else dir)))

(defn follow-instruction-part1 [{ pos :pos dir :dir} instruction]
  {:pos (apply-pos-instruction-part1 instruction pos dir) :dir (apply-dir-instruction-part1 instruction dir)})

(defn follow-instructions-part1 [instructions]
  (reduce follow-instruction-part1 {:pos [0 0] :dir east-dir} instructions))

(def instructions (map parse-instruction (get-input-lines)))

(defn abs [a] (if (< a 0) (- a) a))

(defn -main
  [& args]
  (let [{[x y] :pos dir :dir} (follow-instructions-part1 instructions)]
    (println "Part 1:")
    (println (+ (abs x) (abs y)))
    (println "Part 2:")
    (println " .. solution .. ")))
