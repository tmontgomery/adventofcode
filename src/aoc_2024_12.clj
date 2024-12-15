(ns aoc-2024-12)

(defn parse
  [rdr]
  (let [plot-map (into [] (line-seq rdr))]
    [(reduce
       (fn [plots [x y]] (conj plots [x y]))
       #{}
       (for [x (range (count (get plot-map 0)))
             y (range (count plot-map))] [x y]))
     plot-map]))

(defn possible-neighbors
  [plots plot-map x y letter]
  (reduce
    (fn [neighbors [x y]]
      (if (and (= letter (get-in plot-map [y x]))
               (contains? plots [x y]))
        (conj neighbors [x y])
        neighbors))
    #{}
    [[(+ x -1) (+ y 0)]
     [(+ x 0) (+ y 1)]
     [(+ x 1) (+ y 0)]
     [(+ x 0) (+ y -1)]]))

(defn form-shape
  [plots shape plot-map start-x start-y letter]
  (loop [plots plots
         shape shape
         coords #{[start-x start-y]}]
    (if (empty? coords)
      [plots shape]
      (let [[plots shape coords]
            (reduce
              (fn [[plots shape coords] [x y]]
                [(disj plots [x y])
                 (conj shape [x y])
                 (clojure.set/union coords (possible-neighbors plots plot-map x y letter))])
              [plots shape #{}]
              coords)]
        (recur plots
               shape
               coords)))))

(defn num-fences
  [plot-map x y letter]
  (reduce
    (fn [sum [x y]]
      (if (not= (get-in plot-map [y x]) letter)
        (inc sum)
        sum))
    0
    [[(+ x -1) (+ y 0)]
     [(+ x 0) (+ y 1)]
     [(+ x 1) (+ y 0)]
     [(+ x 0) (+ y -1)]]))

(defn price
  [plot-map shape]
  (let [fences (reduce
          (fn [sum [x y]]
            (+ sum (num-fences plot-map x y (get-in plot-map [y x]))))
          0
          shape)]
    (* fences (count shape))))

(defn all-shapes
  [plots plot-map]
  (loop [plots plots
         shapes []]
    (if (empty? plots)
      shapes
      (let [[x y] (first plots)
            letter (get-in plot-map [y x])
            [plots shape] (form-shape plots [] plot-map x y letter)]
        (recur plots
               (conj shapes shape))))))

(time (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-12-input.txt")]
        (let [[plots plot-map] (parse rdr)
              shapes (all-shapes plots plot-map)]
          (reduce #(+ %1 (price plot-map %2)) 0 shapes))))