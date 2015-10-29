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

(defn row-with-step? [elements step]
  (let [sorted (sort elements) 
        size (count sorted)]
    ;;(println sorted)
    (loop [others (rest sorted)
           prev (first sorted)]
      ;;(println "tail " others)
      (if (not-empty others)
        (let [cur (first others)]
          (if (= (- cur prev) step)
            (recur (rest others) cur)
            false))
        true))))

(defn straight? [hand]
  (let [rank-freqs (apply dissoc (count-ranks hand) [1 14])
        freqs (set (vals rank-freqs))]
    (if (not= (count freqs) 1)
      false
      (row-with-step? (keys rank-freqs) 1))))

(defn straight-flush? [hand]
  (let [rank-freqs (apply dissoc (count-ranks hand) [1 14])
        suits (set (keys (count-suits hand)))
        freqs (set (vals rank-freqs))]
    ;;(println hand)
    (if (not= (count freqs) 1)
      false
      (and
        (= (count suits) 1)
        (row-with-step? (keys rank-freqs) 1)))))

;;(straight-flush? ["2H" "4H" "5H" "9H" "7H"])
(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers {
         high-card? 0
         pair? 1
         two-pairs? 2
         three-of-a-kind? 3
         straight? 4
         flush? 5
         full-house? 6
         four-of-a-kind? 7
         straight-flush? 8}]
    (apply max
           (filter 
             (fn [checked] checked)
               (map 
                 (fn [checker] 
                   (let [[fcheck v] checker]
                     (if (fcheck hand) v -1)))
                 checkers)))))

;;(value ["2H" "4H" "5H" "9H" "7H"])




