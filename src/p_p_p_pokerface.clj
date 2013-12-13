(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12  \K 13 \A 14}
        [rank _] card]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (get replacements rank))))

(defn suit [card]
  (str (get card 1)))

(defn rank-count [hand]
  (vals (frequencies (map rank hand))))

(defn has-num-same-cards? [numcards hand]
  (== (apply max (rank-count hand)) numcards))

(defn pair? [hand]
  (has-num-same-cards? 2 hand))

(defn three-of-a-kind? [hand]
  (has-num-same-cards? 3 hand))

(defn four-of-a-kind? [hand]
  (has-num-same-cards? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [rank-list (sort (rank-count hand))]
    (= (range 2 4) rank-list)))

(defn two-pairs? [hand]
  (let [rank-list (sort (rank-count hand))]
    (or
     (four-of-a-kind? hand)
     (= [1 2 2] (sort (rank-count hand))))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
