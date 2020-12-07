(ns day07.core
  (:require clojure.set)
  (:gen-class))

(defn get-input-lines []
  (clojure.string/split-lines (slurp "..\\..\\input\\day07.txt")))

(defn parse-int
  ([s] (Integer/parseInt s))
  ([s radix] (Integer/parseInt s radix)))

(defn parse-containee [containee-spec]
  (let [[count color] (clojure.string/split containee-spec #" " 2)]
    {:count (parse-int count) :color color }))

(defn parse-bag-spec
  [spec-line]
  (let [contains-regex #"^([a-z ]+)? bags contain ((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?((\d+ [a-z ]+?) bags?[.,] ?)?$"
        ;groups don't seem to allow capturing multiple instances?
        capture-groups (re-find contains-regex spec-line)
        [container & containees] (filter some? (map #(get capture-groups %) (range 1 15 2)))
        containees-parsed (map parse-containee containees)]
    {:container container :containees containees-parsed}))

(def bag-rules (map parse-bag-spec (get-input-lines)))

(def bag-rules-map (apply merge (map (fn [{container :container containees :containees}] {container containees}) bag-rules)))

(def contained-by
  (group-by :color
    (mapcat 
      (fn [{container :container containees :containees}]
        (map 
          (fn [containee] {:color (:color containee) :container container})
          containees))
      bag-rules)))

(defn possible-containers
  ([color] (possible-containers [color] #{}))
  ([[color & remaining] seen]
    (if (nil? color)
      seen
      ; else
      (let [containers (set (map :container (get contained-by color)))
            new-containers (clojure.set/difference containers seen)]
        (recur (into remaining new-containers) (into seen new-containers))))))

(defn must-contain [color]
  (let [containees (get bag-rules-map color)
        get-count (fn [{count :count color :color}] (+ count (* count (must-contain color))))]
    (apply + (map get-count containees))))

(defn -main
  [& args]
  (let [bag-color "shiny gold"]
    (println "Part 1:")
    (println (count (possible-containers bag-color)))
    (println "Part 2:")
    (println (must-contain bag-color))))
