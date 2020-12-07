(ns day06.core
  (:require [clojure.set])
  (:gen-class))

(defn get-input-groups
  []
  (clojure.string/split (slurp "..\\..\\input\\day06.txt") #"\r\n\r\n" ))

(defn get-distinct-answers-count [group]
  (count (remove #{\return \newline} (set group))))

(defn get-counts-sum [count-fn groups]
  (apply + (map count-fn groups)))

(defn get-common-answers-count [group]
  (let [lines (clojure.string/split-lines group)
        sets (map set lines)
        common-answers (apply clojure.set/intersection sets)]
    (count common-answers)))

(defn -main
  [& args]
  (let [puzzle-input (get-input-groups)]
    (println (get-counts-sum get-distinct-answers-count puzzle-input))
    (println (get-counts-sum get-common-answers-count puzzle-input))))
