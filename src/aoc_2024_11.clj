(ns aoc-2024-11)

(defn parse
  [rdr]
  (clojure.string/split (clojure.string/trim-newline (slurp rdr)) #" "))

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
        (conj new-arrangement "1")

        (even? (count stone))
        (-> new-arrangement
            (conj (trim-leading-0s (subs stone 0 (int (/ (count stone) 2)))))
            (conj (trim-leading-0s (subs stone (Math/ceil (/ (count stone) 2))))))

        :else
        (conj new-arrangement (str (* 2024 (Long/parseLong stone)))))
      )
    []
    arrangement))

(time (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-11-input.txt")]
        (let [arrangement (parse rdr)]
          (println arrangement)
          (count (loop [blinks 0
                  arrangement arrangement]
                   (println "blink" blinks)
             (if (= blinks 25)
               arrangement
               (recur (inc blinks)
                      (blink arrangement))))))))
