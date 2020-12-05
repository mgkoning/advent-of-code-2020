(ns day05.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day05.txt")))

(def puzzle-input (get-input-lines))

(defn find-binary [lo hi max instructions]
  (let [step (fn [{min :min max :max} instr]
                (let [half (+ min (quot (- max min) 2))]
                  (if (= lo instr) 
                    {:min min :max half}
                    {:min (+ half 1) :max max})))]
    (:min (reduce step {:min 0 :max max} instructions))))

(defn find-seat [instructions]
  (find-binary \L \R 7 instructions))

(defn find-row [instructions]
  (find-binary \F \B 127 instructions))

(defn to-seat [seating-spec]
  (let [row-spec (take 7 seating-spec)
        seat-spec (drop 7 seating-spec)
        row (find-row row-spec)
        seat (find-seat seat-spec)]
    {:row row :seat seat :id (+ seat (* row 8))}))

(defn find-missing [sorted-seats]
  (let [[a & [b & _ :as rest]] sorted-seats
        next (+ 1 a)]
    (if (= b next) (recur rest) next)))

(defn -main
  [& args]
  (let [seats (map to-seat puzzle-input)
        seat-ids (map :id seats)]
    (println "Part 1:")
    (println (reduce max seat-ids))
    (println "Part 2:")
    (println (find-missing (sort seat-ids)))))

