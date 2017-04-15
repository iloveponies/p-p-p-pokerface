(ns p-p-p-pokerface)

(def rank-int {\A 14, \K 13, \Q 12, \J 11, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})
(def low-ace-rank-int {\A 1, \K 13, \Q 12, \J 11, \T 10, \9 9, \8 8, \7 7, \6 6, \5 5, \4 4, \3 3, \2 2})

(defn rank [card]
  (let [[rank _] card]
    (rank-int rank)))

(defn rank-low-ace [card]
  (let [[rank _] card]
    (low-ace-rank-int rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn card-ranks [hand]
  (sort (map rank hand)))

(defn card-ranks-low-ace [hand]
  (sort (map rank-low-ace hand)))

(defn card-suits [hand]
  (map suit hand))

(defn max-freq-rank? [hand n]
  (let [freqs (vals (frequencies (card-ranks hand)))]
    (== n (apply max freqs))))

(defn max-freq-suit? [hand n]
  (let [freqs (vals (frequencies (card-suits hand)))]
    (== n (apply max freqs))))

(defn strictly-rising-monotonic? [a-seq]
  (apply < a-seq))

(defn pair? [hand]
  (max-freq-rank? hand 2))

(defn three-of-a-kind? [hand]
  (max-freq-rank? hand 3))

(defn four-of-a-kind? [hand]
  (max-freq-rank? hand 4))

(defn flush? [hand]
  (max-freq-suit? hand 5))

(defn full-house? [hand]
  (let [freqs (sort (vals (frequencies (card-ranks hand))))]
    (= '(2 3) freqs)))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (card-ranks hand))))]
    (= '(1 2 2) freqs)))


(defn straight? [hand]
  (or
    (let [ranks (card-ranks hand)]
      (and
        (== 4 (- (last ranks) (first ranks)))
        (strictly-rising-monotonic? ranks)))
    (let [ranks (card-ranks-low-ace hand)]
      (and
        (== 4 (- (last ranks) (first ranks)))
        (strictly-rising-monotonic? ranks)))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)     8
   (four-of-a-kind? hand)     7
   (full-house? hand)         6
   (flush? hand)              5
   (straight? hand)           4
   (three-of-a-kind? hand)    3
   (two-pairs? hand)          2
   (pair? hand)               1
   :else                      0))



