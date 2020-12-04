(ns day04.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day04.txt")))

(defn words [x] (clojure.string/split x #" "))

(defn key-val [x] (clojure.string/split x #":"))

(defn make-passport
  [passport-lines]
  (apply concat (map words passport-lines)))

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

(defn is-valid-pass1
  [passport-items]
  (let [has-item (fn [item]
                    (some identity (map #(clojure.string/starts-with? % item) passport-items)))]
    (every? identity (map has-item ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))))

(defn is-range [lo hi candidate]
  (let [val (Integer. candidate)]
    (and (<= lo val) (<= val hi))))

(defn valid-hgt? [hgt]
  (let [[_ val unit] (re-find #"(\d+)+(in|cm)" hgt)]
    (cond
      (= "in" unit) (is-range 59 76 val)
      (= "cm" unit) (is-range 150 193 val)
      :else false)))

(def validators 
  {"byr" (partial is-range 1920 2002)
   "iyr" (partial is-range 2010 2020)
   "eyr" (partial is-range 2020 2030)
   "hgt" valid-hgt?
   "hcl" (partial re-matches #"#[0-9a-f]{6}")
   "ecl" (partial re-matches #"(amb|blu|brn|gry|grn|hzl|oth)")
   "pid" (partial re-matches #"[0-9]{9}")
   "cid" (fn [x] true)})

(defn is-valid-item
  [item]
  (let [[key val] (key-val item)]
    (not (not ((get validators key) val)))))

(defn is-valid-pass2
  [passport-items]
  (every? is-valid-item passport-items))

(defn -main
  [& args]
  (let [passports (read-passports (get-input-lines))
        valid-pass1 (filter is-valid-pass1 passports)]
    (println "Part 1:")
    (println (count valid-pass1))
    (println "Part 2:")
    (println (count (filter is-valid-pass2 valid-pass1)))))

(defn check-passport [val]
  (let [passport (make-passport val)]
    (and (is-valid-pass1 passport) (is-valid-pass2 passport))))