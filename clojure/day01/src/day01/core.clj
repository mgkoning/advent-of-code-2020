(ns day01.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day01.txt")))

(defn read-int [s] (Integer. s))

(def puzzle-input (map read-int (get-input-lines)))

(defn pairs
  [[x & xs]]
  (if (nil? xs)
    '()
    (concat (map list (repeat x) xs) (pairs xs))))

(defn part1
  [[head & tail]]
  (let [[a b] head
        found (= 2020 (+ a b))]
    (if found (* a b) (recur tail))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (pairs puzzle-input))))
