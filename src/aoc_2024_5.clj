(ns aoc-2024-5)

(def test-input
  ["47|53"
   "97|13"
   "97|61"
   "97|47"
   "75|29"
   "61|13"
   "75|53"
   "29|13"
   "97|29"
   "53|29"
   "61|53"
   "97|53"
   "61|29"
   "47|13"
   "75|47"
   "97|75"
   "47|61"
   "75|61"
   "47|29"
   "75|13"
   "53|13"
   ""
   "75,47,61,53,29"
   "97,61,53,29,13"
   "75,29,13"
   "75,97,47,61,53"
   "61,13,29"
   "97,13,75,29,47"])

(defn parse
  [rdr]
  (reduce
    (fn [res line]
      (condp = (get line 2)
        \|
        (let [[dep page] (clojure.string/split line #"\|")]
          [(update (first res) page (fnil conj #{}) dep) (second res)])
        \,
        [(first res) (conj (second res) (clojure.string/split line #"\,"))]
        res)
      )
    [{} []]
    (into [] (line-seq rdr))))

(defn is-ordered?
  [rules ordering]
  (reduce (fn [res index]
            (if (empty? (clojure.set/intersection (set (subvec ordering (inc index)))
                                                  (rules (get ordering index))))
              (and res true)
              false)) true (range (count ordering))))

(defn value [ordering] (Integer/parseInt (get ordering (int (/ (count ordering) 2)))))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-5-input.data")]
  (let [[rules orderings] (parse rdr)]
    (reduce
      (fn [sum ordering]
        (if (is-ordered? rules ordering)
          (+ (value ordering) sum)
          sum)) 0 orderings)))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-5-input.data")]
  (let [[rules orderings] (parse rdr)]
    (reduce
      (fn [sum ordering]
        (if (is-ordered? rules ordering)
          sum
          (+ sum (value (vec (sort #(contains? (get rules %2) %1) ordering)))))) 0 orderings)))
