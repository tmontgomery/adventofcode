(ns aoc-2024-2)

(def test-nums
  '([7 6 4 2 1]
   [1 2 7 8 9]
   [9 7 6 2 1]
   [1 3 2 4 5]
   [8 6 4 4 1]
   [1 3 6 7 9]))

(defn is-safe?
  [nums]
  (let [pairs (partition 2 1 nums)]
   (and (or (every? (fn [[a b]]
                   (> a b))
                 pairs)
         (every? (fn [[a b]]
                   (< a b))
                 pairs))
        (every? (fn [[a b]]
                  (and (>= (- a b) -3)
                       (<= (- a b) 3)))
                pairs))))

(defn can-be-safe?
  [nums]
 (or (is-safe? nums)
     (reduce (fn [a b]
               (println a b)
               (or a b))
             (for [idx (range (count nums))]
               (is-safe? (vec (concat (subvec (vec nums) 0 idx) (subvec (vec nums) (inc idx)))))))))

(let [v [7 6 4 2 1]]
  (for [idx (range (count v))]
   (println (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))))

(let [nums [7 6 4 2 1]]
  (reduce (fn [a b]
           (or a b))
         (for [idx (range (count nums))]
           (is-safe? (vec (concat (subvec nums 0 idx) (subvec nums (inc idx))))))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-2-input.data")]
    (reduce
      (fn [s line]
        (let [nums (map #(Integer/parseInt %) (clojure.string/split line #" "))]
          (println nums (can-be-safe? nums))
          (if (can-be-safe? nums) (inc s) s)))
      0
      (into [] (line-seq rdr))))
