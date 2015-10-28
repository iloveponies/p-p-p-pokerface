(ns p-p-p-pokerface)

(defn rank [card]
  (let [[s-rank _] card
        rank-map { \T 10 \J 11 \Q 12 \K 13 \A 14 }
        h-rank (get rank-map s-rank)]
    (if h-rank
      h-rank
      (Integer/valueOf (str s-rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn count-suits [hand]
  (frequencies (map suit hand)))

(defn count-ranks [hand]
  (frequencies (map rank hand)))

(defn cutoff-ranks [hand cut-level]
  (filter 
    (fn [freqq] (let [[rank freq] freqq]
                  (> freq cut-level)))
    (count-ranks hand)))

(defn pair? [hand]
  (not (empty? (cutoff-ranks hand 1))))

(defn three-of-a-kind? [hand]
  (not (empty? (cutoff-ranks hand 2))))

(defn four-of-a-kind? [hand]
  (not (empty? (cutoff-ranks hand 3))))

(defn flush? [hand]
  (= (count (count-suits hand))
     1))

(defn contains-count? [freqs cnt]
  (contains?
    (set (vals freqs))
    cnt))

(defn full-house? [hand]
  (let [rank-freqs (count-ranks hand)]
    (and
      (contains-count? rank-freqs 3)
      (= (count rank-freqs) 2))))

(defn two-pairs? [hand]
  (= (count (cutoff-ranks hand 1))
     2))

(defn straight? [hand]
  (let [rank-freqs (dissoc (count-ranks hand) [1 14])
        freqs (set (vals rank-freqs))]
    (if (not= (count freqs) 1)
      false
      (< (sort (keys rank-freqs))))))

(defn row? [collection]
  (let [sorted (sort collection) 
        size (count sorted)]
    ;;(println sorted)
    (loop [i 0 prev nil status true]
      (if (and status (< i size))
        (let [ith (get sorted i)]
          ;;(println (str i ", " ith))
          (if (or (not prev) (= (- ith prev) 1))
            (recur (inc i) ith true)
            false))))))


(row? [1 2 7 4 3])
(row? [1 2 3 4 5])

(defn straight-flush? [hand]
  (let [rank-freqs (dissoc (count-ranks hand) [1 14])
        suits (set (keys (count-suits hand)))
        freqs (set (vals rank-freqs))]
    (println hand)
    (if (not= (count freqs) 1)
      false
      (and
        (= (count suits) 1)
        (< (sort (keys rank-freqs)))))))

(defn value [hand]
  nil)
