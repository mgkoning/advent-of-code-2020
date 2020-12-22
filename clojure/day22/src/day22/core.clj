(ns day22.core
  (:gen-class))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s r] (Long/parseLong s r)))

(defn get-input [] (slurp "..\\..\\input\\day22.txt"))
(defn get-input-lines [] (clojure.string/split-lines (get-input)))
(defn get-input-groups [] (clojure.string/split (get-input) #"\r?\n\r?\n"))

(defn parse-decks [deck-groups]
  (let [decks (map (comp (partial map parse-long) (partial drop 1) clojure.string/split-lines) deck-groups)]
    (map (partial reduce conj clojure.lang.PersistentQueue/EMPTY) decks)))

(defn determine-winner [deck1 deck2]
  (cond
    (empty? deck1) deck2
    (empty? deck2) deck1
    :else (let [top1 (peek deck1)
                rest1 (pop deck1)
                top2 (peek deck2)
                rest2 (pop deck2)]
            (if (< top1 top2)
              (recur rest1 (conj rest2 top2 top1))
              (recur (conj rest1 top1 top2) rest2)))))

(defn determine-score [winner]
  (reduce + 0 (map * (seq winner) (range (count winner) 0 -1))))

(defn -main
  [& args]
  (let [[deck1, deck2] (parse-decks (get-input-groups))
        winner (determine-winner deck1 deck2)
        score (determine-score winner)]
    (println "Part 1:")
    (println score)
    (println "Part 2:")
    (println " ..")))

(def test-input ["Player 1:
9
2
6
3
1"
  
"Player 2:
5
8
4
7
10"])