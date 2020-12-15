(ns day15.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day15.txt")))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s radix] (Long/parseLong s radix)))

(def puzzle-input (map parse-long (clojure.string/split (first (get-input-lines)) #",")))

(defn rest-numbers [mem prev-turn prev-number]
  (let [last-heard (mem prev-number)
        next-number (if (nil? last-heard) 0 (- prev-turn last-heard))
        next-mem (assoc mem prev-number prev-turn)]
    (lazy-seq (cons next-number (rest-numbers next-mem (inc prev-turn) next-number)))))

(defn memory-chain [starting-numbers]
  (let [previous-number (last starting-numbers)
        memory-map (apply hash-map (mapcat (fn [num turn] [num turn]) starting-numbers (drop 1 (range))))
        turn (count starting-numbers)]
    (concat starting-numbers (rest-numbers memory-map turn previous-number))))

(defn number-at [sequence index]
  (nth sequence (dec index)))

(defn -main
  [& args]
  (let [recitation (memory-chain puzzle-input)]
    (println "Part 1:")
    (println (number-at recitation 2020))
    (println "Part 2:")
    (println (number-at recitation 30000000))))
