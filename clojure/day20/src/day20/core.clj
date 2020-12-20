(ns day20.core
  (:gen-class))

(defn parse-long [s]
  (Long/parseLong s))

(defn combinations
    "Creates all combinations of n items from a list"
    [n [x & xs :as all]]
    (if (= 1 n)
      (map list all)
      (if (nil? xs) 
        '()
        (concat (map cons (repeat x) (combinations (- n 1) xs)) (combinations n xs)))))
  
(def pairs (partial combinations 2))
  
(defn get-input [] (slurp "..\\..\\input\\day20.txt"))

(defn parse-tile [tile-spec]
  (let [[header & lines] (clojure.string/split-lines tile-spec)
        tile-no (parse-long (subs header 5 (- (count header) 1)))
        edges [(first lines) (last lines) (apply str (map first lines)) (apply str (map last lines))]
        reversed-edges (map #(apply str (reverse %)) edges)]
    {tile-no, (concat edges reversed-edges)}))

(defn get-tiles [input]
  (let [groups (clojure.string/split input #"\r?\n\r?\n")
        tiles (apply merge (map parse-tile groups))]
    tiles))

(defn find-matches [tiles-map [tile1 tile2]]
  (let [pairs (for [e1 (tiles-map tile1) e2 (tiles-map tile2)] [e1, e2])]
    (if (some (fn [[e1 e2]] (= e1 e2)) pairs)
      [[tile1, tile2] [tile2, tile1]]
      [])))

(defn determine-corner-tiles [tiles-map]
  (let [tile-nos (keys tiles-map)
        tile-pairs (pairs tile-nos)
        matches (mapcat (partial find-matches tiles-map) tile-pairs)
        grouped-matches (group-by first matches)]
    (filter #(== 2 (count (second %))) grouped-matches)))

(defn -main
  [& args]
  (let [tiles-map (get-tiles (get-input))
        corners (determine-corner-tiles tiles-map)]
    (println "Part 1:")
    (println (apply * (keys corners)))
    (println "Part 2:")
    (println " ..")))

