(ns aoc-2024-9)

(defn parse
  [rdr]
  (let [line (clojure.string/trim-newline (slurp rdr))]
    (loop [index 0
           id 0
           free? false
           disk-map '()
           file-list {}]
      (let [val (get line index)]
        (if (nil? val)
          [disk-map (assoc file-list :max-id (dec id))]
          (recur (inc index)
                 (if free? id (inc id))
                 (if free? false true)
                 (concat disk-map
                         (if free?
                           (repeat (- (int val) 48) nil)
                           (repeat (- (int val) 48) id)))
                 (if free?
                   file-list
                   (assoc file-list id (- (int val) 48)))))))))

(defn trim-file-info
  [file-info id]
  (let [val (get file-info id)]
    (if (<= val 1)
      (cond-> (dissoc file-info id)
            (= (:max-id file-info) id)
            (assoc :max-id (dec id)))
      (update file-info id dec))))

(defn compact-checksum
  [disk-map file-info]
  (loop [idx 0
         checksum (bigdec 0)
         info file-info]
    (if (= 1 (count info))
      checksum
      (if (nil? (get disk-map idx))
        (recur (inc idx)
               (+ checksum (bigdec (* idx (:max-id info))))
               (trim-file-info info (:max-id info)))
        (recur (inc idx)
               (+ checksum (bigdec (* idx (get disk-map idx))))
               (trim-file-info info (get disk-map idx)))))))

(time (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-9-input.txt")]
   (let [[disk-map file-info] (parse rdr)]
     (println (count disk-map) (dec (count file-info)))
     (compact-checksum (vec disk-map) file-info))))

(def unallocated -1)

(defn set-disk-map
  [disk-map starting-index length value]
  (doseq [index (range starting-index (+ starting-index length))]
    (aset-int disk-map index value)))

(defn parse-2
  [rdr]
  (let [line (clojure.string/trim-newline (slurp rdr))
        disk-map (int-array 50 unallocated)]
    (loop [index 0
           id 0
           free? false
           disk-map-index 0]
      (let [val (get line index)]
        (if (nil? val)
          [disk-map disk-map-index]
          (do
            (set-disk-map disk-map disk-map-index (- (int val) 48) (if free? unallocated id))
            (recur (inc index)
                  (if free? id (inc id))
                  (if free? false true)
                  (+ disk-map-index (- (int val) 48)))))))))

(defn num-blocks-allocated
  [disk-map index]
  (let [id (aget disk-map index)]
   (reduce (fn [length offset]
             (if (not= id (aget disk-map (+ index offset)))
               (reduced length)
               (inc length)))
           0
           (for [offset (range 10)] (- offset)))))

(defn is-big-enough?
  [disk-map index length]
  (every? #(= unallocated %)
    (for [i (range length)]
     (aget disk-map (+ index i)))))

(defn find-free-blocks
  [disk-map length max-index]
  (loop [index 0]
    (if (= index max-index)
      -1
      (if ((= unallocated (aget disk-map index)))
        )))
  )

(time (with-open [rdr (clojure.java.io/reader "/Users/tmont/Documents/AoC-2024/day-9-test-input.txt")]
        (let [[disk-map length] (parse-2 rdr)]
          (println length (vec disk-map))
          (println (num-blocks-allocated disk-map 25)))))
