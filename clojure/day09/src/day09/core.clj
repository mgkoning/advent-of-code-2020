(ns day09.core
  (:gen-class))


(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day09.txt")))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s radix] (Long/parseLong s radix)))

(def puzzle-input (map parse-long (get-input-lines)))

(defn combinations
  "Creates all combinations of n items from a list"
  [n [x & xs :as all]]
  (if (= 1 n)
    (map list all)
    (if (nil? xs) 
      '()
      (concat (map cons (repeat x) (combinations (- n 1) xs)) (combinations n xs)))))

(defn find-first-invalid
  ([numbers]
    (let [preamble (take 25 numbers)
          rest (drop 25 numbers)
          previousNumbers (apply (partial conj clojure.lang.PersistentQueue/EMPTY) preamble)]
    (find-first-invalid rest previousNumbers)))
  ([[head & numbers] previousNumbers]
    (let [pairs (combinations 2 previousNumbers)]
      (if (some #(= head (apply + %)) pairs)
        (recur numbers (conj (pop previousNumbers) head))
        head))))

(defn take-until-sum
  ([target numbers] (take-until-sum target numbers []))
  ([target [head & numbers] acc]
    (let [sum (apply + acc)]
      (cond
        (< sum target) (recur target numbers (conj acc head))
        (< target sum) nil
        :else acc))))

(defn tails [[x & rest :as coll]]
  (if (empty? coll)
    []
    (cons (seq coll) (lazy-seq (tails rest)))))

(defn find-sum [target numbers]
  (first (drop-while nil? (map (partial take-until-sum target) (tails numbers)))))

(defn -main
  [& args]
  (let [first-invalid (find-first-invalid puzzle-input)]
    (println "Part 1:")
    (println first-invalid)
    (println "Part 2:")
    (let [sum-range (find-sum first-invalid puzzle-input)
          minimum (apply min sum-range)
          maximum (apply max sum-range)]
      (println (+ minimum maximum)))))

