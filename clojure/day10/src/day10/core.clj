(ns day10.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day10.txt")))

(defn parse-int [s] (Integer/parseInt s))

(def puzzle-input (map parse-int (get-input-lines)))

(defn find-equal [x coll] (filter #(= x %) coll))

(defn get-differences [ratings]
  (let [max-rating (apply max ratings)
        device-rating (+ 3 max-rating)
        all-ratings (cons 0 (cons device-rating ratings))
        sorted-ratings (sort all-ratings)
        differences (map - (drop 1 sorted-ratings) sorted-ratings)]
  differences))

(defn part1 [ratings]
  (let [differences (get-differences ratings)]
    (println differences)
    (* (count (find-equal 3 differences)) (count (find-equal 1 differences)))))
    
    

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 puzzle-input)))
