(ns aoc-2024-8)

(defn parse
  [rdr]
  (let [map (into [] (line-seq rdr))]
    [(reduce
       (fn [tower-map [x y]]
         (if (not= \. (get-in map [y x]))
           (update tower-map (get-in map [y x]) (fnil conj []) [x y])
           tower-map))
       {}
       (for [x (range (count (get map 0)))
             y (range (count map))] [x y]))
     map]))

(defn all-pairs
  [coord-list]
  (for [a (range (count coord-list))
        b (range (count coord-list))
        :when (not= a b)] [a b]))

(defn antinode
  [map node-1 node-2]
  (let [diff (clojure.core/map - node-1 node-2)
        [antinode-x antinode-y] (clojure.core/map + node-1 diff)]
    (when (not (nil? (get-in map [antinode-y antinode-x])))
      [antinode-x antinode-y])))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-8-input.data")]
  (let [[tower-map map] (parse rdr)]
    (- (count (reduce (fn [antinodes frequency]
                      (reduce (fn [total [node-1 node-2]]
                                (conj total (antinode map
                                                      (get-in tower-map [frequency node-1])
                                                      (get-in tower-map [frequency node-2]))))
                              antinodes
                              (all-pairs (get tower-map frequency))))
                    #{}
                    (keys tower-map))) 1)))