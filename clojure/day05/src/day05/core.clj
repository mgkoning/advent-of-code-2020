(ns day05.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day05.txt")))

(defn parse-int
  ([s] (Integer/parseInt s))
  ([s radix] (Integer/parseInt s radix)))

(def puzzle-input (get-input-lines))

(defn parse-pseudo-binary [zero one val]
  (let [binary-string (clojure.string/replace (clojure.string/replace val one \1) zero \0)]
    (parse-int binary-string 2)))

; old "solution": I shouldn't take the instructions so literally
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
  (let [row-spec (subs seating-spec 0 7)
        seat-spec (subs seating-spec 7)
        row (parse-pseudo-binary \F \B row-spec)
        seat (parse-pseudo-binary \L \R seat-spec)]
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

