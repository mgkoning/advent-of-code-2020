(ns day19.core
  (:gen-class))

(defn get-input [] (slurp "..\\..\\input\\day19.txt"))
  

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-sequence [s]
  (map parse-int (clojure.string/split s #" ")))

(defn parse-rule [line]
  (let [[rule-id rule-spec] (clojure.string/split line #": ")
        rule (if (clojure.string/starts-with? rule-spec "\"")
                {:value (subs rule-spec 1 (- (count rule-spec) 1))}
                {:options (map parse-sequence (clojure.string/split rule-spec #" \| "))})]
    {(parse-int rule-id) rule}))

(defn parse-input [input]
  (let [[rules-group messages-group] (clojure.string/split input #"\r?\n\r?\n")
        rules (map parse-rule (clojure.string/split-lines rules-group))
        messages (clojure.string/split-lines messages-group)]
    [(apply merge rules) messages]))

(defn all-options [[head & tail]]
  (if (nil? head)
    (list ())
    (for [prefix head suffix (all-options tail)] (cons prefix suffix))))

(defn expand [rules-map rule]
  (letfn [(expand-option [parts]
            (let [expanded-parts (map #(expand rules-map (rules-map %)) parts)]
              (map #(apply str %) (all-options expanded-parts))))]
    (if (:value rule)
      [(:value rule)]
      (mapcat expand-option (:options rule)))))


(defn matching-messages [match-op prefixes suffixes messages]
  (letfn [(is-match? [m]
            (let [parts (map #(apply str %) (partition-all 8 m))
                  prefix-matches (count (take-while #(contains? prefixes %) parts))
                  suffix-matches (count (take-while #(contains? suffixes %) (drop prefix-matches parts)))]
              (and
                (= (count parts) (+ prefix-matches suffix-matches))
                (match-op 2 prefix-matches)
                (match-op 1 suffix-matches)
                (< suffix-matches prefix-matches))))]
    (count (filter is-match? messages))))

(def part1 (partial matching-messages =))
(def part2 (partial matching-messages <=))

(defn -main
  [& args]
  (let [[rules-map messages] (parse-input (get-input))
        prefixes (into #{} (expand rules-map (rules-map 42)))
        suffixes (into #{} (expand rules-map (rules-map 31)))]
    (println "Part 1:")
    (println (part1 prefixes suffixes messages))
    (println "Part 2:")
    (println (part2 prefixes suffixes messages))))
