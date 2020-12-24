(ns day24.core
  (:require clojure.set)
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

(defn get-black-tiles [input-lines]
  (let [step-lines (map parse-steps input-lines)
        final-positions (map #(reduce vec+ [0 0 0] (map moves %)) step-lines)
        position-counts (apply merge-with + (map #(hash-map % 1) final-positions))
        odd-visits (filter #(odd? (second %)) position-counts)]
    (set (map first odd-visits))))

(defn get-neighbors [t]
  (map #(vec+ t %) hex-neighbors))

(defn flip-tiles [black-tiles]
  (let [all-tiles-to-visit (concat black-tiles (mapcat get-neighbors black-tiles))]
    (letfn [(consider-tile [result v] 
              (let [neighbors (get-neighbors v)
                    black-neighbors (count (filter #(contains? black-tiles %) neighbors))
                    is-black (contains? black-tiles v)
                    will-be-black (or (== 2 black-neighbors) (and is-black (== 1 black-neighbors)))]
                (if will-be-black (conj result v) result)))]
      (reduce consider-tile #{} all-tiles-to-visit))))

(defn -main
  [& args]
  (let [black-tiles (get-black-tiles (get-input-lines))]
    (println "Part 1:")
    (println (count black-tiles))
    (println "Part 2:")
    (println (nth (map count (iterate flip-tiles black-tiles)) 100))))

(def test-input (clojure.string/split-lines
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"))