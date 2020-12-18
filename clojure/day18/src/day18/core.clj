(ns day18.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day18.txt")))

(def lparen (first "("))
(def rparen (first ")"))

(defn parse-expr
  ([expr] (parse-expr expr nil nil))
  ([[cursor & rest] acc op]
    (let [consume (fn [val] (if (some? op) (op acc val) val))]
      (cond
        (or (= cursor rparen) (= cursor nil)) [acc rest]
        (= cursor \*) (recur rest acc *)
        (= cursor \+) (recur rest acc +)
        (= cursor lparen)
          (let [[subexpr remaining] (parse-expr rest)]
            (recur remaining (consume subexpr) nil))
        (= cursor \space) (recur rest acc op)
        :else (recur rest (consume (Long/parseLong (str cursor))) nil)))))

(defn part1 [lines]
  (apply + (map #(first (parse-expr %)) lines)))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (get-input-lines))
  (println "Part 2:")
  (println " ... ")))
