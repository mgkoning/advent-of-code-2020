(ns day05.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day05.txt")))

(defn parse-int
  ([s] (Integer/parseInt s))
  ([s radix] (Integer/parseInt s radix)))

(def puzzle-input (get-input-lines))

(defn parse-pseudo-binary [zeroes ones val]
  (let [ones-replaced (reduce #(clojure.string/replace %1 %2 \1) val ones)
        zeroes-replaced (reduce #(clojure.string/replace %1 %2 \0) ones-replaced zeroes)]
    (parse-int zeroes-replaced 2)))

; old "solution": I shouldn't take the instructions so literally
(comment "all obsolete"
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
          row (parse-pseudo-binary [\F] [\B] row-spec)
          seat (parse-pseudo-binary [\L] [\R] seat-spec)]
      {:row row :seat seat :id (+ seat (* row 8))}))

  (defn find-missing [sorted-seats]
    (let [[a & [b & _ :as rest]] sorted-seats
          next (+ 1 a)]
      (if (= b next) (recur rest) next))))

; "one pass" solution: determine min, max and sum of all seats, as that's all that's required for
; the puzzle answers
(defn one-pass [seat-specs]
  (let [step (fn [[sum min-id max-id] seat-spec]
                (let [seat-id (parse-pseudo-binary [\F \L] [\B \R] seat-spec)]
                  [(+ sum seat-id) (min min-id seat-id) (max max-id seat-id)]))]
    (reduce step [0 1024 0] seat-specs)))

(defn -main
  [& args]
  (let [[sum min-id max-id] (one-pass puzzle-input)
        expected-sum (apply + (range min-id (+ 1 max-id)))
        missing-id (- expected-sum sum)]
    (println "Part 1:")
    (println max-id)
    (println "Part 2:")
    (println missing-id)))

