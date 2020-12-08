(ns day08.core
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day08.txt")))

(defn words [x] (clojure.string/split x #" "))


(defn parse-int
  ([s] (Integer/parseInt s))
  ([s radix] (Integer/parseInt s radix)))

(defn parse-instruction [line]
  (let [[op arg] (words line)
        int-arg (parse-int arg)]
    {:op op :arg int-arg}))

(def opcodes
  {"jmp" (fn [arg {ip :ip acc :acc}] {:ip (+ ip arg) :acc acc})
   "acc" (fn [arg {ip :ip acc :acc}] {:ip (+ ip 1) :acc (+ acc arg)})
   "nop" (fn [arg {ip :ip acc :acc}] {:ip (+ ip 1) :acc acc})})

(defn opcode [{op :op arg :arg} state]
  ((get opcodes op) arg state))

(defn step [program {ip :ip acc :acc :as state}]
  (let [op (get program ip)]
    (if (nil? op)
      nil
      (opcode (get program ip) state))))

(def program (vec (map parse-instruction (get-input-lines))))

(def start-state {:ip 0 :acc 0})

(defn run-program [program state]
  (if (nil? state)
    []
    (cons state (lazy-seq (run-program program (step program state))))))

(defn find-loop
  ([steps] (find-loop steps #{}))
  ([[{ip :ip acc :acc} & tail] seen-ips]
    (if (contains? seen-ips ip)
      {:loop true :acc acc}
      (if (nil? tail)
        {:loop false :acc acc}
        (recur tail (conj seen-ips ip))))))

(defn find-halt [program]
  (let [swaps {"jmp" "nop" "nop" "jmp" "acc" "acc"}
        swap-op (fn [program index] (update program index (fn [{op :op arg :arg}] {:op (swaps op) :arg arg})))
        variations (map swap-op (repeat program) (range (count program)))]
    (first (drop-while :loop (map (fn [p] (find-loop (run-program p start-state))) variations)))))

(defn -main
  [& args]
  (let [steps (run-program program start-state)]
    (println "Part 1:")
    (println (:acc (find-loop steps)))
    (println "Part 2:")
    (println (:acc (find-halt program)))))

