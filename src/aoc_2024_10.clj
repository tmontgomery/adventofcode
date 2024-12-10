(ns aoc-2024-10)

(defn parse
  [rdr]
  (into [] (line-seq rdr)))

(defn height-at
  [topo-map x y]
  (get-in topo-map [y x]))

(defn next-height
  [current-height]
  (char (inc (int current-height))))

(defn possible-routes
  [topo-map x y next-height]
  (reduce
    (fn [routes [x y]]
      (if (= next-height (height-at topo-map x y))
        (conj routes [x y])
        routes))
    #{}
   [[(+ x -1) (+ y 0)]
    [(+ x 0) (+ y 1)]
    [(+ x 1) (+ y 0)]
    [(+ x 0) (+ y -1)]]))

(defn num-routes
  [topo-map x y]
  (loop [height \1
         routes (possible-routes topo-map x y height)]
    (println height routes)
    (if (= \9 height)
      (do
        (println x y (count routes))
        (count routes))
      (recur (next-height height)
             (reduce
               (fn [res [x y]]
                 (clojure.set/union res (possible-routes topo-map x y (next-height height))))
               #{}
               routes)))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-10-input.txt")]
  (let [topo-map (parse rdr)]
    (reduce
      (fn [sum [x y]]
        (if (= \0 (height-at topo-map x y))
          (+ sum (num-routes topo-map x y))
          sum))
      0
      (for [y (range (count topo-map))
            x (range (count (get topo-map 0)))] [x y]))))