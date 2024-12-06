(ns aoc-2024-1)

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-1-input.data")]
  (reduce
   (fn [s line]
     (let [strs (clojure.string/split line #"   ")
           l (Integer/parseInt (first strs))
           r (Integer/parseInt (second strs))]
       [(conj (first s) l)
        (conj (second s) r)]))
   [[] []]
   (into [] (line-seq rdr))))

(let [data (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-1-input.data")]
             (reduce
               (fn [s line]
                 (let [strs (clojure.string/split line #"   ")
                       l (Integer/parseInt (first strs))
                       r (Integer/parseInt (second strs))]
                   [(conj (first s) l)
                    (conj (second s) r)]))
               [[] []]
               (into [] (line-seq rdr))))
      left (first data)
      right (second data)]
 (reduce
   (fn [sum [l r]]
     (+ sum (java.lang.Math/abs ^int (- l r))))
   0
   (map vector (sort left) (sort right))))

(let [data (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-1-input.data")]
             (reduce
               (fn [s line]
                 (let [strs (clojure.string/split line #"   ")
                       l (Integer/parseInt (first strs))
                       r (Integer/parseInt (second strs))]
                   [(conj (first s) l)
                    (conj (second s) r)]))
               [[] []]
               (into [] (line-seq rdr))))
      left (first data)
      right (second data)]
  (reduce
    (fn [sum id]
      (+ sum (* id (count (filter #(= % id) right)))))
    0
    left))

(def left [3 4 2 1 3 3])
(def right [4 3 5 3 9 3])

