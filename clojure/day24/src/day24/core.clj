(ns day24.core
  (:gen-class))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s r] (Long/parseLong s r)))

(defn get-input [] (slurp "..\\..\\input\\day24.txt"))
(defn get-input-lines [] (clojure.string/split-lines (get-input)))
(defn get-input-groups [] (clojure.string/split (get-input) #"\r?\n\r?\n"))

(defn parse-steps [line]
  (loop [[a & [b & bs :as rest]] line steps []]
    (cond 
      (nil? a) steps
      (some #{a} #{\e \w}) (recur rest (conj steps (str a)))
      (some #{a} #{\n \s}) (recur bs (conj steps (str a b)))
      :else (throw (Exception. (str "Don't know " a))))))

(defn vec+ [a b] (vec (map + a b)))

(def moves {
  "w"  [-1  1  0]
  "nw" [ 0  1 -1]
  "ne" [ 1  0 -1]
  "e"  [ 1 -1  0]
  "se" [ 0 -1  1]
  "sw" [-1  0  1]
})

(def hex-neighbors (vals moves))

(defn get-black-tiles [tiles]
  (let [position-counts (apply merge-with + (map #(hash-map % 1) tiles))
        odd-visits (filter #(odd? (second %)) position-counts)]
    (map first odd-visits)))

(defn -main
  [& args]
  (let [step-lines (map parse-steps (get-input-lines))
        final-positions (map #(reduce vec+ [0 0 0] (map moves %)) step-lines)
        black-tiles (get-black-tiles final-positions)]
    (println "Part 1:")
    (println (count black-tiles))
    (println "Part 2:")
    (println " ...")))
