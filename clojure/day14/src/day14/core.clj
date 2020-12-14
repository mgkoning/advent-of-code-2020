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

(defn get-mask [line]
  (if (clojure.string/starts-with? line "mask = ") (subs line 7) nil))

(defn get-assignment [line]
  (let [[_ address-str value-str] (clojure.string/split line #"\[|\] = ")]
    [(parse-long address-str) (parse-long value-str)]))

(defn process-part1 [[mem [base-value mask :as mask-state]] line]
  (let [mask-value (get-mask line)]
    (cond
      (some? mask-value) [mem (read-mask mask-value)]
      :else
        (let [[address value] (get-assignment line)]
          [(assoc mem address (bit-or (bit-and value mask) base-value)) mask-state]))))

(defn part1 [input-lines]
  (let [[mem _] (reduce process-part1 [{} [0 0]] input-lines)]
    (apply + (vals mem))))

(defn pad-to-36 [value]
  (str (apply str (repeat (- 36 (count value)) \0)) value))

(defn apply-mask [mask address]
  (map (fn [m b] (cond (= \0 m) b :else m)) mask address))

(defn get-all-addresses [masked-address]
  (let [process-bit (fn [addresses bit]
                      (if (not= \X bit)
                        (map #(conj % bit) addresses)
                        (mapcat (fn [address] [(conj address \0) (conj address \1)]) addresses)))]
    (map (partial apply str) (reduce process-bit [[]] masked-address))))

(defn process-part2 [[mem mask] line]
  (let [mask-value (get-mask line)]
    (if
      (some? mask-value) [mem mask-value]
      ;else
      (let [[address value] (get-assignment line)
            masked-address (apply-mask mask (pad-to-36 (Long/toString address 2)))
            all-addresses (map #(parse-long % 2) (get-all-addresses masked-address))
            new-memory-state (reduce #(assoc %1 %2 value) mem all-addresses)]
        [new-memory-state mask]))))

(defn part2 [input-lines]
  (let [[mem _] (reduce process-part2 [{} ""] input-lines)]
    (apply + (vals mem))))

(defn -main
  [& args]
  (println "Part 1:")
  (println (part1 (get-input-lines)))
  (println "Part 2:")
  (println (part2 (get-input-lines))))

(def test-lines-part1
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
   "mem[8] = 11"
   "mem[7] = 101"
   "mem[8] = 0"])

(def test-lines-part2
  ["mask = 000000000000000000000000000000X1001X"
   "mem[42] = 100"
   "mask = 00000000000000000000000000000000X0XX"
   "mem[26] = 1"])