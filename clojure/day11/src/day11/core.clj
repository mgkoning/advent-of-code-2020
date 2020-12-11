(ns day11.core
  (:gen-class))

(defn get-input [] (slurp "..\\..\\input\\day11.txt"))

(defn get-seating [seating]
  (let [lines (clojure.string/split-lines seating)
        seat-map (apply
                   merge
                   (mapcat (fn [row y] (map (fn [val x] {[x y] val}) row (range))) lines (range)))]
    seat-map))

(defn get-neighbours [[x y]]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)] [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn new-seat [seat-map [pos current]]
  (let [neighbours (get-neighbours pos)
        occupied-neighbours (apply + (filter some? (map (fn [n] (if (= \# (seat-map n)) 1 0)) neighbours)))
        new-value (cond 
          (and (= current \L) (= 0 occupied-neighbours)) \#
          (and (= current \#) (<= 4 occupied-neighbours)) \L
          :else current)]
    {pos new-value}))

(defn next-seat-map [seat-map]
  (into {} (map (partial new-seat seat-map) seat-map)))

(defn seat-evolution [seat-map]
  (cons seat-map (lazy-seq (seat-evolution (next-seat-map seat-map)))))

(defn find-stable [[x & [y & _ :as rest]]]
  (if (= x y) x (find-stable rest)))

(defn -main
  [& args]
  (println "Part 1:")
  (println (count (filter #(= \# %) (vals (find-stable (seat-evolution (get-seating (get-input))))))))
  (println "Part 2:")
  (println ".. solution .."))

(def test-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")