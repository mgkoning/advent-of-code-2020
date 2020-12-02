(ns day02.core
  (:gen-class))

(defn get-input-lines
  []
  (clojure.string/split-lines (slurp "..\\..\\input\\day02.txt")))

(defn read-int [s] (Integer. s))

(defn get-password-record
  [line]
  (let [matcher (re-matcher #"(\d+)-(\d+) ([a-z]): ([a-z]+)" line)
        _ (re-find matcher)
        properties (drop 1 (re-groups matcher))
        [lo hi char pw] properties]
    {:lo (read-int lo) :hi (read-int hi) :char (.charAt char 0) :password pw}))

(def puzzle-input (map get-password-record (get-input-lines)))

(defn valid-password-part1?
  [{lo :lo
    hi :hi
    char :char
    password :password}]
  (let [charCount (count (filter #(= char %) password))]
    (and (<= charCount hi) (<= lo charCount))))

(defn valid-passwords
  [valid-fn input]
  (count (filter valid-fn input)))

(defn -main
  [& args]
  (println "Part 1")
  (println (valid-passwords valid-password-part1? puzzle-input)))
