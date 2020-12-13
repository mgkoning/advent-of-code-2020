(ns day13.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day13.txt")))

(defn parse-long [s] (Long/parseLong s))

(defn part1 [[earliest-line timetable-line]]
  (let [earliest (parse-long earliest-line)
        bus-lines (map parse-long (filter #(not= "x" %) (clojure.string/split timetable-line #",")))
        after-last-departure (map (fn [bus] [bus (rem earliest bus)]) bus-lines)
        wait-times (map (fn [[bus remainder]] [bus (- bus remainder)]) after-last-departure)]
    (apply * (first (sort-by second wait-times)))))

(defn get-bus-lines [[_ timetable-line]]
  (let [parse-bus (fn [id] (if (= "x" id) nil (parse-long id)))
        bus-lines (map parse-bus (clojure.string/split timetable-line #","))
        indexed-bus-lines (map (fn [index id] [index id]) (range) bus-lines)]
    (filter #(some? (second %)) indexed-bus-lines)))

(defn part2 [input]
  (let [indexed-bus-lines (get-bus-lines input)
        is-ok-for-id (fn [n [index id]] (= 0 (rem (+ n index) id)))]
    (loop [start-at 0
           step (second (first indexed-bus-lines))
           prefix-length 2]
      (let [current-bus-lines (take prefix-length indexed-bus-lines)
             ; since these are all primes, the least common multiple of all of them (i.e., the next
             ; step size) is their product.
            next-step (apply * (map second current-bus-lines))
            steps (range start-at Long/MAX_VALUE step)
            first-match (first (filter #(every? (partial is-ok-for-id %) current-bus-lines) steps))]
        (if (= prefix-length (count indexed-bus-lines))
          first-match
          (recur first-match next-step (+ 1 prefix-length)))))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (get-input-lines)))
  (println "Part 2:")
  (println (part2 (get-input-lines))))

(defn test-part2 []
  (println (part2 ["" "7,13,x,x,59,x,31,19"]))
  (println (part2 ["" "17,x,13,19"]))
  (println (part2 ["" "67,7,59,61"]))
  (println (part2 ["" "67,x,7,59,61"]))
  (println (part2 ["" "67,7,x,59,61"]))
  (println (part2 ["" "1789,37,47,1889"])))