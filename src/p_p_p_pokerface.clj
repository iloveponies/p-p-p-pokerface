(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ r _ ] card
        rank-values { \T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
          (Integer/valueOf (str r))
          (rank-values r))))

(defn suit [card]
  (let [[ _ s ] card]
    (str s)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
