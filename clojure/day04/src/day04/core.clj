(ns day04.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day04.txt")))

(defn make-passport
  [passport-lines]
  (apply concat (map #(clojure.string/split %  #" ") passport-lines)))

(defn read-passports
  [lines]
  (loop [remaining lines
         passport-lines []
         passports []]
    (if (empty? remaining)
      (conj passports (make-passport passport-lines))
      (let [[line & rest] remaining]
        (if (empty? line)
          (recur rest [] (conj passports (make-passport passport-lines)))
          (recur rest (conj passport-lines line) passports))))))

(defn is-valid
  [passport-items]
  (let [has-item
         (fn [item] (some identity (map #(clojure.string/starts-with? % item) passport-items)))]
    (every? identity (map has-item ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))))
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Part 1:")
  (let [passports (read-passports (get-input-lines))]
    (println (count (filter identity (map is-valid passports))))))
