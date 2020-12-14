(ns day14.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day14.txt")))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s radix] (Long/parseLong s radix)))

(defn read-mask [mask-bits]
  (let [base-value (parse-long (clojure.string/replace mask-bits \X \0) 2)
        mask (parse-long (clojure.string/replace (clojure.string/replace mask-bits \1 \0) \X \1) 2)]
    [base-value mask]))

(defn process [[mem [base-value mask :as mask-state]] line]
  (cond
    (clojure.string/starts-with? line "mask = ") [mem (read-mask (subs line 7))]
    :else
      (let [[_ address-str value-str] (clojure.string/split line #"\[|\] = ")
            address (parse-long address-str)
            value (parse-long value-str)]
        [(assoc mem address (bit-or (bit-and value mask) base-value)) mask-state])))

(defn part1 [input-lines]
  (let [[mem _] (reduce process [{} [0 0]] input-lines)]
    (apply + (vals mem))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (get-input-lines)))
  (println "Part 2:")
  (println " .... "))

(def test-lines
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
   "mem[8] = 11"
   "mem[7] = 101"
   "mem[8] = 0"])