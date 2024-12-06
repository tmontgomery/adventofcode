(ns aoc-2024-4)

(def test-1-input
  ["MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"])

(def xmas-offsets
  [['(0 0) '(-1 0) '(-2 0) '(-3 0)]
   ['(0 0) '(-1 -1) '(-2 -2) '(-3 -3)]
   ['(0 0) '(0 -1) '(0 -2) '(0 -3)]
   ['(0 0) '(1 -1) '(2 -2) '(3 -3)]
   ['(0 0) '(1 0) '(2 0) '(3 0)]
   ['(0 0) '(1 1) '(2 2) '(3 3)]
   ['(0 0) '(0 1) '(0 2) '(0 3)]
   ['(0 0) '(-1 1) '(-2 2) '(-3 3)]])

(defn xmas-matches
  [matrix x y]
  (reduce (fn [r line]
            (let [s (reduce (fn [string coordinate]
                              (let [x-pos (+ x (first coordinate))
                                    y-pos (+ y (second coordinate))
                                    c (get-in matrix [y-pos x-pos])]
                                (str string c)))
                            ""
                            line)]
              (if (= "XMAS" s) (inc r) r)))
          0
          xmas-offsets))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-4-input.data")]
  (let [matrix (into [] (line-seq rdr))]
    (println (count (get matrix 0)) (count matrix))
    (reduce
      (fn [sum [x y]]
        (if (= \X (get-in matrix [y x]))
          (+ sum (xmas-matches matrix x y))
          sum))
      0
      (for [pos-x (range (count (get matrix 0)))
            pos-y (range (count matrix))]
        [pos-x pos-y]))))

(defn x-mas-matches
  [matrix x y]
  (let [s (str (get-in matrix [(- y 1) (- x 1)] \.)
               (get-in matrix [(- y 1) (+ x 1)] \.)
               (get-in matrix [y x] \.)
               (get-in matrix [(+ y 1) (- x 1)] \.)
               (get-in matrix [(+ y 1) (+ x 1)] \.))]
    (condp = s
      "MMASS" 1
      "SSAMM" 1
      "MSAMS" 1
      "SMASM" 1
      0)))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-4-input.data")]
  (let [matrix (into [] (line-seq rdr))]
    (println (count (get matrix 0)) (count matrix))
    (reduce
      (fn [sum [x y]]
        (if (= \A (get-in matrix [y x]))
          (+ sum (x-mas-matches matrix x y))
          sum))
      0
      (for [pos-x (range (count (get matrix 0)))
            pos-y (range (count matrix))]
        [pos-x pos-y]))))

