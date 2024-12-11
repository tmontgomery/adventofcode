(ns aoc-2024-11)

(defn parse
  [rdr]
  (reduce
    (fn [arrangement stone]
      (update arrangement stone (fnil inc 0)))
    {}
    (clojure.string/split (clojure.string/trim-newline (slurp rdr)) #" ")))

(reverse (vec (range 5)))

(defn trim-leading-0s
  [s]
  (let [num-leading-0s (reduce
                          (fn [sum idx]
                            (if (not (= \0 (get s idx)))
                              (reduced sum)
                              (inc sum)))
                          0
                          (range (count s)))]
    (if (= (count s) num-leading-0s)
      "0"
      (subs s num-leading-0s))))

(defn blink
  [arrangement]
  (reduce
    (fn [new-arrangement stone]
      (cond
        (= "0" stone)
        (update new-arrangement "1" (fnil + 0) (get arrangement stone))

        (even? (count stone))
        (-> new-arrangement
            (update (trim-leading-0s (subs stone 0 (int (/ (count stone) 2)))) (fnil + 0) (get arrangement stone))
            (update (trim-leading-0s (subs stone (Math/ceil (/ (count stone) 2)))) (fnil + 0) (get arrangement stone)))

        :else
        (update new-arrangement (str (* 2024 (Long/parseLong stone))) (fnil + 0) (get arrangement stone)))
      )
    {}
    (keys arrangement)))

(time (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-11-input.txt")]
        (let [arrangement (parse rdr)]
          (println arrangement)
          (reduce +
            (vals (loop [blinks 0
                    arrangement arrangement]
               (println "blink" blinks)
               (if (= blinks 75)
                 arrangement
                 (recur (inc blinks)
                        (blink arrangement)))))))))
