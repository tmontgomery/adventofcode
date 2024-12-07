(ns aoc-2024-7)

(defn parse
  [rdr]
  (reduce
      (fn [res line]
        (conj res (map #(Long/parseLong %) (re-seq #"\d+" line))))
      []
      (into [] (line-seq rdr))))

(defn has-solution?
   [test-value number-list]
   (-> (reduce (fn [results num]
                 (reduce (fn [this-time total]
                           (-> this-time
                               (conj (+ total num))
                               (conj (* total num))))
                         #{}
                         results))
               #{(first number-list)}
               (rest number-list))
       (contains? test-value)))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-7-input.data")]
  (let [equations (parse rdr)]
    (reduce
      (fn [sum equation]
        (if (has-solution? (first equation) (rest equation))
          (+ sum (first equation))
          sum))
      0
      equations)))

(defn has-solution-2?
  [test-value number-list]
  (-> (reduce (fn [results num]
                (reduce (fn [this-time total]
                          (-> this-time
                              (conj (+ total num))
                              (conj (* total num))
                              (conj (Long/parseLong (str total num)))))
                        #{}
                        results))
              #{(first number-list)}
              (rest number-list))
      (contains? test-value)))

(with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-7-input.data")]
  (let [equations (parse rdr)]
    (reduce
      (fn [sum equation]
        (if (has-solution-2? (first equation) (rest equation))
          (+ sum (first equation))
          sum))
      0
      equations)))
