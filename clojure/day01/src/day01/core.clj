(ns day01.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day01.txt")))

(defn read-int [s] (Integer. s))

(def puzzle-input (map read-int (get-input-lines)))

(defn combinations
  "Creates all combinations of n items from a list"
  [n [x & xs :as all]]
  (if (= 1 n)
    (map list all)
    (if (nil? xs) 
      '()
      (concat (map cons (repeat x) (combinations (- n 1) xs)) (combinations n xs)))))

(def pairs (partial combinations 2))

(def triples (partial combinations 3))

(defn find-desired-sum
  [target [head & tail]]
  (let [found (= target (apply + head))]
    (if found (apply * head) (recur target tail))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (find-desired-sum 2020 (pairs puzzle-input)))
  (println "Part 2:")
  (println (find-desired-sum 2020 (triples puzzle-input))))
