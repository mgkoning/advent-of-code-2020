(ns day20.core
  (:require clojure.set)
  (:gen-class))

(defn parse-long [s]
  (Long/parseLong s))

(defn print-tile [tile]
  (println (clojure.string/join "\n" tile)))

(defn vec+ [a b] (map + a b))

(defn combinations
    "Creates all combinations of n items from a list"
    [n [x & xs :as all]]
    (if (= 1 n)
      (map list all)
      (if (nil? xs) 
        ()
        (concat (map cons (repeat x) (combinations (dec n) xs)) (combinations n xs)))))
  
(def pairs (partial combinations 2))

(defn get-input [] (slurp "..\\..\\input\\day20.txt"))

(defn get-image-size [image] [(count (first image)) (count image)])

(def sea-monster-pattern [
"                  # "
"#    ##    ##    ###"
" #  #  #  #  #  #   "])

(def monster-image-size (get-image-size sea-monster-pattern))

(defn indicize [image]
  (for [y (range 0 (count image))
       :let [row (get image y)]
       x (range 0 (count row))]
    [[x y] (get row x)]))

(defn get-only-coords [indicized]
  (into #{} (map first (filter #(= \# (second %)) indicized))))

(def sea-monster-coords
  (get-only-coords (indicize sea-monster-pattern)))

(defn parse-tile [tile-spec]
  (let [[header & lines] (clojure.string/split-lines tile-spec)
        tile-no (parse-long (subs header 5 (dec (count header))))]
    {tile-no, lines}))

(defn get-edges [image]
  (let [edges [(first image) (last image) (apply str (map first image)) (apply str (map last image))]
        reversed-edges (map #(apply str (reverse %)) edges)]
    (concat edges reversed-edges)))

(defn get-tiles-map [input]
  (into {} (map parse-tile (clojure.string/split input #"\r?\n\r?\n"))))

(defn get-edges-map [tiles]
  (into {} (map (fn [[tile-no image]] {tile-no, (get-edges image)}) tiles)))

(defn find-matches [tiles-map [tile1 tile2]]
  (let [pairs (for [e1 (tiles-map tile1) e2 (tiles-map tile2) :when (= e1 e2)] [e1, e2])]
    (if (seq pairs)
      [[tile1, tile2, pairs] [tile2, tile1, pairs]]
      [])))

(defn match-tiles [tiles-map]
  (let [tile-nos (keys tiles-map)
        tile-pairs (pairs tile-nos)
        matches (mapcat (partial find-matches tiles-map) tile-pairs)
        grouped-matches (group-by first matches)]
    grouped-matches))

(defn flip-v [image] (map #(apply str (reverse %)) image))

(defn transpose [[[x & xs :as head] & xss :as image]]
  (if (empty? image)
    ()
    (if (empty? head)
      (recur xss)
      (let [first (cons x (map first xss))
            rest (transpose (cons xs (map rest xss)))]
        (cons first rest)))))

(defn rotate [image] (flip-v (transpose image)))

(defn trim-border [image]
  (map #(subs % 1 (dec (count %))) (drop 1 (drop-last image))))

(defn stitch [tile-locations]
  (let [sorted-tiles (into (sorted-map) (group-by #(second (first %)) (sort-by #(first (first %)) tile-locations)))
        stitched (map #(apply str %) (mapcat (fn [[y tiles]] (apply (partial map concat) (map (comp trim-border second) tiles))) sorted-tiles))]
    (vec stitched)))

(defn get-adjacent [[x y]]
  (map (fn [[dx dy p]] [(+ x dx) (+ y dy) p]) [[-1 0 \L] [1 0 \R] [0 -1 \T] [0 1 \B]]))

(def edge-direction-map {\T 0 \B 1 \L 2 \R 3})
(def edge-companion-map {\T \B \B \T \L \R \R \L})
(defn all-orientations [image]
  (reductions #(%2 %1) image [rotate rotate rotate flip-v rotate rotate rotate]))

(defn fit-tile [placed to-place [x y edge-dir :as place]]
  (let [edge (nth (get-edges placed) (edge-direction-map edge-dir))
        companion-edge (edge-direction-map (edge-companion-map edge-dir))
        possible-fits (filter #(= edge (nth (get-edges %) companion-edge)) (all-orientations to-place))]
    (map (fn [fit] [place fit]) possible-fits)))

(defn fits-all-neighbors [tile-locations [[x y _] image]]
  (let [adjacents (get-adjacent [x y])
        my-edges (get-edges image)
        pos-map (into {} (vals tile-locations))]
    (every? (fn [[x y dir]]
              (or
                (nil? (pos-map [x y])) 
                (= 
                  (nth my-edges (edge-direction-map dir))
                  (nth (get-edges (pos-map [x y])) (edge-direction-map (edge-companion-map dir))))))
            adjacents)))

(defn print-tile-locations [tile-locations]
  (count (map (fn [[pos image]] (println pos) (print-tile image) (println "")) tile-locations)))

(defn assemble [tiles-map tiles-matches start-from]
  (letfn [(get-neighbors [tile] (map second (tiles-matches tile)))
          (place-tile [[tile-locations available-places tile] to-place]
            (let [tile-to-place (tiles-map to-place)
                  possible-fits (mapcat #(fit-tile tile tile-to-place %) available-places)
                  [[x y _ :as place] orientation] (first (filter #(fits-all-neighbors tile-locations %) possible-fits))]
              [(assoc tile-locations to-place [[x y] orientation]) (remove #{place} available-places) tile]))]
    (loop [tile-locations {start-from [[0 0] (tiles-map start-from)]}
           to-do #{start-from}]
      (if (empty? to-do)
        (stitch (vals tile-locations))
        (let [[next & remaining] to-do
              neighbors (clojure.set/difference (set (get-neighbors next)) (set (keys tile-locations)))
              [position tile] (tile-locations next)
              placement-options (get-adjacent position)
              taken-places (set (map first (vals tile-locations)))
              available-places (filter (fn [[x y _]] (not (contains? taken-places [x y]))) placement-options)
              [new-tile-locations & _] (reduce place-tile [tile-locations available-places tile] neighbors)]
          (recur new-tile-locations (into remaining neighbors)))))))

(defn find-monsters [image]
  (let [[size-x size-y] (get-image-size image)
        dark-coords (get-only-coords (indicize image))
        get-monster-at (fn [top-left]
                         (let [expected-coords (into #{} (map #(vec+ top-left %) sea-monster-coords))
                               intersected (clojure.set/intersection expected-coords dark-coords)]
                            (if (== (count sea-monster-coords) (count intersected))
                              intersected
                              #{})))
        monster-locations (for [x (range 0 size-x) y (range 0 size-y)] (get-monster-at [x y]))]
    (- (count dark-coords) (count (reduce clojure.set/union monster-locations)))))

(defn -main
  [& args]
  (let [tiles-map (get-tiles-map (get-input))
        edges-map (get-edges-map tiles-map)
        tiles-matches (match-tiles edges-map)
        corners (filter #(== 2 (count (second %))) tiles-matches)]
    (println "Part 1:")
    (println (apply * (keys corners)))
    (let [assembled-image (assemble tiles-map tiles-matches (first (keys corners)))]
      (println "Part 2:")
      (println (first (sort (map #(find-monsters (vec %)) (all-orientations assembled-image))))))))
