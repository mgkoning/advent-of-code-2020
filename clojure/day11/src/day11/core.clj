(ns day11.core
  (:gen-class))

(defn get-input [] (slurp "..\\..\\input\\day11.txt"))

(defn get-seating [seating]
  (let [lines (clojure.string/split-lines seating)
        seat-map (apply
                   merge
                   (mapcat (fn [row y] (map (fn [val x] {[x y] val}) row (range))) lines (range)))]
    seat-map))

(defn get-neighbours-part1 [seat-map [x y]]
  (let [adjacents [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                   [x (dec y)] [x (inc y)]
                   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]]]
    (map seat-map adjacents)))

(defn add-coord [[x y] [dx dy]] [(+ x dx) (+ y dy)])

(defn get-neighbours-part2 [seat-map [x y :as pos]]
  (let [directions [[-1 -1] [-1 0] [-1 1]
                    [0 -1] [0 1]
                    [1 -1] [1 0] [1 1]]
        get-nearest (fn [pos direction]
                      (let [candidate-pos (add-coord pos direction)
                            candidate (seat-map candidate-pos)]
                        (if (= \. candidate) (recur candidate-pos direction) candidate)))]
    (map (partial get-nearest pos) directions)))


(defn new-seat [seat-map threshold get-neighbours-fn [pos current]]
  (let [neighbours (get-neighbours-fn seat-map pos)
        occupied-neighbours (apply + (filter some? (map (fn [n] (if (= \# n) 1 0)) neighbours)))
        new-value (cond 
          (and (= current \L) (= 0 occupied-neighbours)) \#
          (and (= current \#) (<= threshold occupied-neighbours)) \L
          :else current)]
    {pos new-value}))

(defn next-seat-map [seat-map threshold get-neighbours-fn]
  (into {} (map (partial new-seat seat-map threshold get-neighbours-fn) seat-map)))

(defn seat-evolution [seat-map threshold get-neighbours-fn]
  (cons seat-map (lazy-seq (seat-evolution (next-seat-map seat-map threshold get-neighbours-fn) threshold get-neighbours-fn))))

(defn find-stable [[x & [y & _ :as rest]]]
  (if (= x y) x (find-stable rest)))

(defn count-occupied [seat-map]
  (count (filter #(= \# %) (vals seat-map))))

(defn -main
  [& args]
  (let [seat-map (get-seating (get-input))]
    (println "Part 1:")
    (println (count-occupied (find-stable (seat-evolution seat-map 4 get-neighbours-part1))))
    (println "Part 2:")
    (println (count-occupied (find-stable (seat-evolution seat-map 5 get-neighbours-part2))))))

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