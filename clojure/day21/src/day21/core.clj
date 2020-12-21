(ns day21.core
  (:require clojure.set)
  (:gen-class))

(defn get-input-lines [] (clojure.string/split-lines (slurp "..\\..\\input\\day21.txt")))

(defn parse-ingredients [line]
  (let [[ingredients-list allergens-list] (clojure.string/split line #" \(contains ")
        ingredients (clojure.string/split ingredients-list #" ")
        allergens (clojure.string/split (subs allergens-list 0 (dec (count allergens-list))) #", ")]
    {:ingredients ingredients :allergens allergens}))

(defn get-allergens-by-food [{ingredients :ingredients allergens :allergens}]
  (map (fn [a] { a (into #{} ingredients) }) allergens))

(defn get-possible-allergens [foods]
  (let [by-food (mapcat get-allergens-by-food foods)]
    (apply (partial merge-with clojure.set/intersection) by-food)))

(defn get-allergen-free [foods possible-allergens]
  (let [all-possible-allergens (reduce clojure.set/union (vals possible-allergens))
        all-ingredients (mapcat :ingredients foods)]
    (remove all-possible-allergens all-ingredients)))

(defn translate-allergens [possible-allergens]
  (loop [remaining possible-allergens known {}]
    (if (empty? remaining)
      known
      (let [single-options (filter #(== 1 (count (second %))) remaining)
            known' (into known (map (fn [[a n]] [(first n) a]) single-options))
            accounted-for (into #{} (keys known'))
            remove-ingredients (fn [[a i]] [a (clojure.set/difference i accounted-for)])
            remaining' (into {} (map remove-ingredients (apply dissoc remaining (vals known'))))]
        (recur remaining' known')))))

(defn -main
  [& args]
  (let [foods (map parse-ingredients (get-input-lines))
        possible-allergens (get-possible-allergens foods)
        allergen-free (get-allergen-free foods possible-allergens)]
    (println "Part 1:")
    (println (count allergen-free))
    (println "Part 2:")
    (let [allergens (translate-allergens possible-allergens)
          sorted-allergens (sort-by val allergens)]
      (println (clojure.string/join "," (map first sorted-allergens))))))

(def test-foods (map parse-ingredients [
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  "trh fvjkl sbzzf mxmxvkd (contains dairy)"
  "sqjhc fvjkl (contains soy)"
  "sqjhc mxmxvkd sbzzf (contains fish)"]))