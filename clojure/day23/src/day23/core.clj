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

(defn move-to [t {[r & rs] :right :as zipper}]
  (if (= t r) zipper (recur t (move-right zipper))))

(defn crab-move [min max {[r & rs] :right :as zipper}]
  (let [[zipper' removed] (remove-right 3 zipper)
        destination (determine-destination r removed min max)
        result (move-right (insert-after destination removed zipper'))]
    result))

(defn play-game [min max moves digits]
  (let [all-moves (iterate (partial crab-move min max) (->Zipper () digits))
        result (first (drop moves all-moves))]
    (move-to 1 result)))

(defn get-nexts [n from next-vec]
  (loop [i 0 at from nexts []]
    (if (<= n i)
      nexts
      (let [next (next-vec at)]
        (recur (inc i) next (conj nexts next))))))

(defn assoc-pair [d [k v]] (assoc d k v))

(defn build-next-vector [digits]
  (let [vect (into (vector-of :int) (repeat (inc (count digits)) 0))]
    (reduce assoc-pair vect (map vector digits (drop 1 (cycle digits))))))

(defn crab-move2 [min max [at next-vec]]
  (let [[next1 next2 next3 next4] (get-nexts 4 at next-vec)
        destination (determine-destination at [next1 next2 next3] min max)
        destination-next (next-vec destination)]
    [next4 (reduce assoc-pair next-vec [[at next4] [destination next1] [next3 destination-next]])]))

(defn play-game2 [min max moves state]
  (let [all-moves (iterate (partial crab-move2 min max) state)
        [_ result] (first (drop moves all-moves))]
    result))

(defn -main
  [& args]
  (let [digits (parse-digits (get-input)) 
        min (reduce min digits)
        max (reduce max digits)
        {ls :left [r & rs] :right} (play-game min max 100 digits)]
    (println "Part 1:")
    (println (str (apply str (reverse ls)) (apply str rs)))
    (let [more-digits (concat digits (range (inc max) (inc 1000000)))
          next-map (build-next-vector more-digits)
          result (play-game2 min 1000000 10000000 [(first digits) next-map])]
      (println (apply * (get-nexts 2 1 result))))))

(def test-input (parse-digits "389125467"))