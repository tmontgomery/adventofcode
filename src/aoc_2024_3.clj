(ns aoc-2024-3)

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def test-2-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-3-input.data")]
  (reduce
    (fn [s line]
      (+ s (reduce
         (fn [line-sum instruction]
           (+ line-sum (reduce * 1 (map #(Integer/parseInt %) (re-seq #"\d+" instruction)))))
         0
         (re-seq #"mul\(\d+,\d+\)+" line))))
    0
    (into [] (line-seq rdr))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-3-input.data")]
  (let [doing? (atom true)]
    (reduce
      (fn [sum instruction]
        (cond
          (= instruction "do()")
          (do (reset! doing? true) sum)

          (= instruction "don't()")
          (do (reset! doing? false) sum)

          :else
          (if @doing?
            (+ sum (reduce * 1 (map #(Integer/parseInt %) (re-seq #"\d+" instruction))))
            sum)))
      0
      (re-seq #"mul\(\d+,\d+\)+|do\(\)|don't\(\)" (slurp rdr)))))
