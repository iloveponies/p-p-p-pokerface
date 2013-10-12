(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13 \A 14}
        [card-rank _] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (get ranks card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
  (str card-suit)))

(defn max-freq [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (< 1 (max-freq hand)))

(defn three-of-a-kind? [hand]
  (< 2 (max-freq hand)))

(defn four-of-a-kind? [hand]
  (< 3 (max-freq hand)))

(defn flush? [hand]
  (== 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
      (full-house? hand)
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks-sorted (sort (map rank hand))
        alt-hand (sort (replace {14 1} ranks-sorted))
        is-straight? (fn [ranks] (= (range (first ranks) (+ (first ranks) 5)) ranks))]
    (or (is-straight? alt-hand) (is-straight? ranks-sorted))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [hand-values {0 (high-card? hand)
                     1 (pair? hand)
                     2 (two-pairs? hand)
                     3 (three-of-a-kind? hand)
                     4 (straight? hand)
                     5 (flush? hand)
                     6 (full-house? hand)
                     7 (four-of-a-kind? hand)
                     8 (straight-flush? hand)
                                         }
        contains-value? (fn [value] (= true (get value 1)))
        values (keys (filter contains-value? hand-values))]
    (apply max values))
  )
