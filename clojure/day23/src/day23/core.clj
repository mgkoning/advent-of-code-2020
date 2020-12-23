(ns day23.core
  (:gen-class))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s r] (Long/parseLong s r)))

(defn get-input [] (slurp "..\\..\\input\\day23.txt"))
(defn get-input-lines [] (clojure.string/split-lines (get-input)))
(defn get-input-groups [] (clojure.string/split (get-input) #"\r?\n\r?\n"))

(defrecord Zipper [left right])

(defn parse-digits [input] (map #(Character/digit % 10) input))

(defn move-right [zipper]
  (let [{ls :left [r & rs] :right} zipper]
    (if (empty? rs)
      (->Zipper () (reverse (cons r ls)))
      (->Zipper (cons r ls) rs))))

(defn remove-right [n zipper]
  (loop [{ls :left [r & rs] :right :as z} zipper taken () i 0]
    (cond
      (<= n i) [z (reverse taken)]
      (empty? rs) (recur (->Zipper () (cons r (reverse ls))) taken i)
      :else (let [[removed & rest] rs]
              (recur (->Zipper ls (cons r rest)) (cons removed taken) (inc i))))))

(defn insert-after-l [v to-insert list]
  (loop [result () [x & xs] list]
    (cond
      (nil? x) nil
      (= v x) (concat (reverse result) (cons x (concat to-insert xs)))
      :else (recur (cons x result) xs))))

(defn insert-before-l [v to-insert list]
  (loop [result () [x & xs :as x-all] list]
    (cond
      (nil? x) nil
      (= v x) (concat (reverse result) to-insert x-all)
      :else (recur (cons x result) xs))))

(defn insert-after [v to-insert {ls :left rs :right :as zipper}]
  (let [inserted-right (insert-after-l v to-insert rs)]
    (if (seq inserted-right)
      (->Zipper ls inserted-right)
      (->Zipper (insert-before-l v (reverse to-insert) ls) rs))))

(defn dec-wrapped [c min max]
  (let [c' (dec c)]
    (if (< c' min) max c')))

(defn determine-destination [c not-allowed min max]
  (let [c' (dec-wrapped c min max)]
    (if (some #{c'} not-allowed) (recur c' not-allowed min max) c')))

(defn crab-move [min max {[r & rs] :right :as zipper}]
  (let [[zipper' removed] (remove-right 3 zipper)
        destination (determine-destination r removed min max)]
    (move-right (insert-after destination removed zipper'))))

(defn move-to [t {[r & rs] :right :as zipper}]
  (if (= t r) zipper (recur t (move-right zipper))))

(defn part1 [digits]
  (let [min (reduce min digits)
        max (reduce max digits)
        all-moves (iterate (partial crab-move min max) (->Zipper () digits))
        result (first (drop 100 all-moves))]
    (move-to 1 result)))

(defn -main
  [& args]
  (let [{ls :left [r & rs] :right} (part1 (parse-digits (get-input)))]
    (println "Part 1:")
    (println (str (apply str (reverse ls)) (apply str rs)))
    (println "Part 2:")
    (println " ..")))

(def test-input (parse-digits "389125467"))