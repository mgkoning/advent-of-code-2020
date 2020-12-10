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
    (* (count (find-equal 3 differences)) (count (find-equal 1 differences)))))

(defn get-one-sequences [differences]
  (if (empty? differences)
    nil
    (let [drop-threes (drop-while #(= 3 %) differences)
          ones (take-while #(= 1 %) drop-threes)
          rest (drop-while #(= 1 %) drop-threes)]
      (cons ones (lazy-seq (get-one-sequences rest))))))


(def factors
  "Possible arrangements of sequences of 1s of length `key`"
  {0 1, 1 1, 2 2, 3 4, 4 7, 5 13})

(defn part2 [ratings]
  (let [differences (get-differences ratings)
        one-sequences (get-one-sequences differences)]
    (apply * (map #(factors (count %)) one-sequences))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 puzzle-input))
  (println "Part 2:")
  (println (part2 puzzle-input)))

(def test1
  (part1 (map parse-int (clojure.string/split-lines "16
10
15
5
1
11
7
19
6
12
4"))))

(def test2
  (part1 (map parse-int (clojure.string/split-lines "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"))))