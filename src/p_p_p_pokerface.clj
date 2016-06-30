(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
              \T 10 \J 11 \Q 12 \K 13 \A 14}
        card-rank (first card)]
    (get ranks card-rank)))

(defn suit [card]
  (str (second card)))

(defn ranks [hand]
  (map rank hand))

(defn suit-unique? [hand]
  (let [suits (set (map suit hand))]
    (== 1 (count suits))))

(defn hand-ranks-freq-sorted [hand]
  (let [hand-freq (frequencies (ranks hand))
        hand-freq-vals (sort (vals hand-freq))]
    hand-freq-vals))

(defn pair? [hand]
  (= [1 1 1 2] (hand-ranks-freq-sorted hand)))

(defn three-of-a-kind? [hand]
  (= [1 1 3] (hand-ranks-freq-sorted hand)))

(defn four-of-a-kind? [hand]
  (= [1 4] (hand-ranks-freq-sorted hand)))

(defn ranks-in-seq? [ranks-seq]
  (let [all-five? (== 5 (count (set ranks-seq)))
        ranks-diff (- (apply max ranks-seq) (apply min ranks-seq))]
    (and all-five? (== 4 ranks-diff))))

(defn flush? [hand]
  (let [ranks-seq (ranks hand)
        ranks-not-seq? (not (ranks-in-seq? ranks-seq))]
    (and ranks-not-seq? (suit-unique? hand))))

(defn full-house? [hand]
  (= [2 3] (hand-ranks-freq-sorted hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (hand-ranks-freq-sorted hand)))

(defn straight-multi? [hand]
  (let [ranks-seq (ranks hand)
        ranks-ace-low (replace {14 1} ranks-seq)]
    (or (ranks-in-seq? ranks-seq) (ranks-in-seq? ranks-ace-low))))

(defn straight? [hand]
  (and (not (suit-unique? hand)) (straight-multi? hand)))

(defn straight-flush? [hand]
  (and (suit-unique? hand) (straight-multi? hand)))

(defn value [hand]
  (cond
    (pair? hand) 1
    (two-pairs? hand) 2
    (three-of-a-kind? hand) 3
    (straight? hand) 4
    (flush? hand) 5
    (full-house? hand) 6
    (four-of-a-kind? hand) 7
    (straight-flush? hand) 8
    :else 0))
