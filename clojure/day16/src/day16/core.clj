(ns day16.core
  (:gen-class))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s radix] (Long/parseLong s radix)))

(defn get-input-file []
  (slurp "..\\..\\input\\day16.txt"))

(defn parse-array [s]
  (map parse-long (clojure.string/split s #",")))

(defn parse-rule [s]
  (let [[property & ranges] (clojure.string/split s #": |-| or ")
        [from1 to1 from2 to2] (map parse-long ranges)
        check-fn (fn [x] (or (and (<= from1 x) (<= x to1)) (and (<= from2 x) (<= x to2))))]
    {property check-fn}))

(defn parse-input [input]
  (let [[rules-block my-ticket-block other-tickets-block] (clojure.string/split input #"\r?\n\r?\n")
        rules-map (apply merge (map parse-rule (clojure.string/split-lines rules-block)))
        my-ticket (parse-array (second (clojure.string/split-lines my-ticket-block)))
        other-tickets (map parse-array (drop 1 (clojure.string/split-lines other-tickets-block)))]
    [rules-map my-ticket other-tickets]))

(defn validate [rules-map ticket]
  (let [is-invalid-value? (fn [v] (not-any? #(% v) (vals rules-map)))
        invalid-values (filter is-invalid-value? ticket)]
    [ticket invalid-values]))

(defn possible-properties [rules-map & [values]]
  (keys (filter (fn [[key check-fn]] (every? check-fn values)) rules-map)))

(defn -main
  [& args]
  (let [[rules-map my-ticket other-tickets] (parse-input (get-input-file))
        validated-tickets (map (partial validate rules-map) other-tickets)
        invalid-tickets (filter #(seq (second %)) validated-tickets)
        valid-tickets (cons my-ticket (map first (filter #(empty? (second %)) validated-tickets)))]
    (println "Part 1:")
    (println (apply + (mapcat second invalid-tickets)))
    (println "Part 2:")
    (println (apply map #(possible-properties rules-map %&) valid-tickets))
    (println "My ticket:")
    (println my-ticket)
    (comment "and then eliminate by hand :-)")
    ))
  
  
