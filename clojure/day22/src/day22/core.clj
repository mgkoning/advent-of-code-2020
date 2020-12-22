(ns day22.core
  (:gen-class))

(defn parse-long
  ([s] (Long/parseLong s))
  ([s r] (Long/parseLong s r)))

(defn get-input [] (slurp "..\\..\\input\\day22.txt"))
(defn get-input-lines [] (clojure.string/split-lines (get-input)))
(defn get-input-groups [] (clojure.string/split (get-input) #"\r?\n\r?\n"))

(defn parse-decks [deck-groups]
  (let [parse-deck (comp (partial map parse-long) (partial drop 1) clojure.string/split-lines)
        decks (map parse-deck deck-groups)]
    (map (partial reduce conj clojure.lang.PersistentQueue/EMPTY) decks)))

(defn determine-winner [player1 player2 is-p1-winner-fn]
  (loop [deck1 player1 deck2 player2 seen-before #{}]
    (cond
      (empty? deck1) [deck2 false]
      (empty? deck2) [deck1 true]
      (contains? seen-before [deck1 deck2]) [deck1 true]
      :else (let [top1 (peek deck1)
                  rest1 (pop deck1)
                  top2 (peek deck2)
                  rest2 (pop deck2)
                  seen-before' (conj seen-before [deck1 deck2])]
              (let [p1wins (is-p1-winner-fn top1 rest1 top2 rest2)]
                (if p1wins
                  (recur (conj rest1 top1 top2) rest2 seen-before')
                  (recur rest1 (conj rest2 top2 top1) seen-before')))))))

(defn determine-score [winner]
  (reduce + 0 (map * (seq winner) (range (count winner) 0 -1))))

(defn is-p1-winner-regular-combat [top1 _ top2 _]
  (< top2 top1))

(defn draw-new-deck [deck count]
  (apply (partial conj clojure.lang.PersistentQueue/EMPTY) (take count deck)))

(defn is-p1-winner-recursive-combat [top1 deck1 top2 deck2]
  (let [can-recurse (fn [top deck] (<= top (count deck)))
        players-can-recurse (and (can-recurse top1 deck1) (can-recurse top2 deck2))
        play-recursive (fn [] (determine-winner 
                                (draw-new-deck deck1 top1)
                                (draw-new-deck deck2 top2)
                                is-p1-winner-recursive-combat))]
  (cond
    players-can-recurse (let [[_ p1-won] (play-recursive)] p1-won)
    :else (< top2 top1))))

(defn -main
  [& args]
  (let [[deck1, deck2] (parse-decks (get-input-groups))
        [winner, p1-won] (determine-winner deck1 deck2 is-p1-winner-regular-combat)]
    (println "Part 1:")
    (println (determine-score winner))
    (let [[winner, p1-won] (determine-winner deck1 deck2 is-p1-winner-recursive-combat)]
      (println "Part 2:")
      (println (determine-score winner)))))
    

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

(def infinite-game-input ["Player 1:
43
19"
  
"Player 2:
2
29
14"])