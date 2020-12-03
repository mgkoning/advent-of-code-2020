(ns day03.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day03.txt")))

(def test-input
  (clojure.string/split-lines
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"))

(def puzzle-input (get-input-lines))

(defn bumble-down
  [{right :right down :down} map]
  (let [bump? (fn [{hor-pos :hor-pos ver-pos :ver-pos bumps :bumps} line] 
                (let [should-check? (= 0 (mod ver-pos down))
                      new-pos (mod (+ hor-pos (if should-check? right 0)) (count line))
                      hit? (and (= \# (get line hor-pos)) should-check?)]
                  {:hor-pos new-pos 
                   :ver-pos (+ ver-pos 1)
                   :bumps (+ bumps (if hit? 1 0))}))]
    (:bumps (reduce bump? {:hor-pos 0 :ver-pos 0 :bumps 0} map))))
  

(defn -main
  [& args]
  (println "Part 1:")
  (println (bumble-down {:right 3 :down 1} puzzle-input))
  (println "Part 2:")
  (let [slopes [{:right 1 :down 1} {:right 3 :down 1} {:right 5 :down 1} {:right 7 :down 1} {:right 1 :down 2}]]
    (println (apply * (map (fn [slope] (bumble-down slope puzzle-input)) slopes)))))
