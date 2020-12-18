(ns day18.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day18.txt")))

(defn parse-digit [c]
  (Long/parseLong (str c)))

; just here so VS Code won't screw up my paren matching :(
(def lparen (first "("))
(def rparen (first ")"))

(def operators {\+ + \* *})
(defn is-operator? [c] (contains? operators c))
(def precedences-p1 {\+ 1 \* 1})
(def precedences-p2 {\+ 9 \* 1})
(def precedences-reg {\+ 1 \* 9})

(defn apply-ops [[a b & values :as value-stack] [op & ops]]
  (if (nil? op)
    value-stack
    (recur (cons ((operators op) a b) values) ops)))

; simplified Shunting-yard algorithm (https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
; apply operators directly rather than collecting them for output
(defn parse-expr-shunting [precedences expr]
  (loop [[cursor & rest] expr
         output-stack ()
         op-stack ()]
    (letfn [(take-ops-to-apply [c [op & ops :as all-ops] popped]
              (cond
                (= lparen op) [(reverse popped), all-ops]
                (nil? op) [(reverse popped), ()]
                (<= (precedences c) (precedences op)) (recur c ops (cons op popped))
                :else [(reverse popped), all-ops]))]
      (cond
        (= \space cursor) (recur rest output-stack op-stack)
        (nil? cursor) (apply-ops output-stack op-stack)
        (= lparen cursor) (recur rest output-stack (cons lparen op-stack))
        (= rparen cursor)
          (let [ops-to-apply (take-while #(not= lparen %) op-stack)
                remaining-ops (drop 1 (drop-while #(not= lparen %) op-stack))]
            (recur rest (apply-ops output-stack ops-to-apply) remaining-ops))
        (is-operator? cursor)
          (let [[ops-to-apply remaining-ops] (take-ops-to-apply cursor op-stack ())]
            (recur rest (apply-ops output-stack ops-to-apply) (cons cursor remaining-ops)))
        :else (recur rest (cons (parse-digit cursor) output-stack) op-stack)))))

(defn math-homework [precedences lines]
  (apply + (map #(first (parse-expr-shunting precedences %)) lines)))

(defn -main
  [& args]
  (let [input-lines (get-input-lines)]
  (println "Part 1:")
  (println (math-homework precedences-p1 input-lines))
  (println "Part 2:")
  (println (math-homework precedences-p2 input-lines))
  (println "Part 3 (regular precedence):")
  (println (math-homework precedences-reg input-lines))))
