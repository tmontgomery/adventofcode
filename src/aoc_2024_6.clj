(ns aoc-2024-6)

(def test-input
  ["....#....."
   ".........#"
   ".........."
   "..#......."
   ".......#.."
   ".........."
   ".#..^....."
   "........#."
   "#........."
   "......#..."])

(defn parse
  [rdr]
  (let [map (into [] #_test-input (line-seq rdr))]
    [(reduce
       (fn [res y]
         (let [x (clojure.string/index-of (get map y) \^)]
           (if (nil? x) res [x y])))
       []
       (range (count map))) map]))

(defn environ [map [x y]] (get-in map [y x]))

(defn turn-right-90 [direction]
  (condp = direction
    '(0 -1) '(1 0)
    '(1 0) '(0 1)
    '(0 1) '(-1 0)
    '(-1 0) '(0 -1)))

(defn patrol
  [map [start-location start-direction]]
  (loop [location start-location
         direction start-direction
         visited-telemetry #{[start-location start-direction]}
         result #{start-location}]
    (let [new-location (clojure.core/map + location direction)
          tile (environ map new-location)]
      (if (contains? visited-telemetry [new-location direction])
        nil
        (cond
          (= \# tile)
          (recur location
                 (turn-right-90 direction)
                 (conj visited-telemetry [location (turn-right-90 direction)])
                 result)

          (or (= \. tile) (= \^ tile))
          (recur new-location
                 direction
                 (conj visited-telemetry [new-location direction])
                 (conj result new-location))

          :else
          result)))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-6-input.data")]
  (let [[location map] (parse rdr)
        direction '(0 -1)]
    (count (patrol map [location direction]))))

(defn update-map
  [map x y c]
  (let [row (get map y)]
    (assoc map y (str (subs row 0, x) c (subs row (inc x))))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-6-input.data")]
  (let [[location map] (parse rdr)
        direction '(0 -1)]
    (reduce
      (fn [sum [x y]]
        (if (and (not= (get-in map [y x]) \#) (not= location [x y]))
          (if (nil? (patrol (update-map map x y \#) [location direction])) (inc sum) sum)
          sum))
      0
      (for [x (range (count (get map 0)))
            y (range (count map))] [x y]))))
