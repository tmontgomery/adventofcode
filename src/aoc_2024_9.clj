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
