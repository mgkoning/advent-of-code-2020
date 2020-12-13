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

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (get-input-lines))))
