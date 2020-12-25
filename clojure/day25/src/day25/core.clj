(ns day25.core
  (:gen-class))

(defn parse-long [s] (Long/parseLong s))

(defn get-input [] (slurp "..\\..\\input\\day25.txt"))
(defn get-input-lines [] (clojure.string/split-lines (get-input)))
(defn get-input-groups [] (clojure.string/split (get-input) #"\r?\n\r?\n"))

(defn handshake-iteration [subject-no value]
  (rem (* value subject-no) 20201227))

(defn generate-values [subject-no]
  (iterate (partial handshake-iteration subject-no) 1))

(defn find-loop-size [[pk1 pk2]]
  (let [generated-values (generate-values 7)]
    (loop [[v & vs] generated-values i 0]
      (cond
        (== v pk1) [i pk1 pk2]
        (== v pk2) [i pk2 pk1]
        :else (recur vs (inc i))))))

(defn -main
  [& args]
  (let [[pk1 pk2] (map parse-long (get-input-lines))
        [loop-size matched-key other-key] (find-loop-size [pk1 pk2])
        encryption-key (nth (generate-values other-key) loop-size)]
    (println "Part 1:")
    (println encryption-key)
    (println "Part 2:")
    (println "Nope!")))
