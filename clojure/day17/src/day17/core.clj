(ns day17.core
  (:require [clojure.set])
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day17.txt")))

(defn get-start-grid [input-lines]
  (let [get-row-coords (fn [row y] (map (fn [val x] {[x y 0 0] val}) row (range)))
        parsed-grid (apply merge (mapcat get-row-coords input-lines (range)))]
    (into #{} (map first (filter #(= \# (second %)) parsed-grid)))))

(defn neighbors3d [[x y z _]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] [(+ x dx) (+ y dy) (+ z dz) 0]))

(defn neighbors4d [[x y z w]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1]] [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn evolve [neighbors-fn conway]
  (letfn [(should-be-active [pos]
            (let [is-active (contains? conway pos)
                  active-neighbors (filter #(contains? conway %) (remove #{pos} (neighbors-fn pos)))
                  active-neighbors-count (count active-neighbors)]
              (cond 
                (and (not is-active) (= 3 active-neighbors-count)) true
                (and is-active (= 2 active-neighbors-count)) true
                (and is-active (= 3 active-neighbors-count)) true
                :else false)))
          (evolve' [[seen result] pos]
            (let [to-process (clojure.set/difference (set (neighbors-fn pos)) seen)
                  to-activate (filter should-be-active to-process)]
              [(into seen to-process) (into result to-activate)]))]
    (let [[_ new-state] (reduce evolve' [#{} #{}] conway)]
      new-state)))

(defn -main
  [& args]
  (println "Part 1:")
  (println (count (nth (iterate (partial evolve neighbors3d) (get-start-grid (get-input-lines))) 6)))
  (println "Part 2:")
  (println (count (nth (iterate (partial evolve neighbors4d) (get-start-grid (get-input-lines))) 6))))

(def test-input [
  ".#."
  "..#"
  "###"])