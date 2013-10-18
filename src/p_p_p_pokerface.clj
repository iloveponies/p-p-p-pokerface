(ns p-p-p-pokerface)

(defn rank [[r s]]
  (def rep {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit r) (Integer/valueOf (str r)) (rep r)))

(defn suit [[r s]]
  (str s))

(defn suit-occurs-more-often-than? [hand, count]
  (> (apply max (vals (frequencies (map suit hand)))) count))

(defn rank-occurs-more-often-than? [hand, count]
  (> (apply max (vals (frequencies (map rank hand)))) count))

(defn pair? [hand]
  (rank-occurs-more-often-than? hand 1))

(defn three-of-a-kind? [hand]
  (rank-occurs-more-often-than? hand 2))

(defn four-of-a-kind? [hand]
  (rank-occurs-more-often-than? hand 3))

(defn flush? [hand]
  (suit-occurs-more-often-than? hand 4))

(defn are-ranks-composed-of? [hand, rank-composition]
  (= rank-composition (vals (frequencies (map rank hand)))))

(defn are-suits-composed-of? [hand, suit-composition]
  (= suit-composition (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
  (are-ranks-composed-of? hand [3 2]))

(defn two-pairs? [hand]
  (are-ranks-composed-of? hand [2 2 1]))

(defn straight? [hand]
  (def suits (sort (map rank hand)))
  (= suits (seq [(min suits) (+ (min suits) 5)])))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
