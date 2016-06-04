(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn freq-ranks [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (let [ranks (freq-ranks hand)]
    (and
      (= (apply max ranks) 2)
      (= (count ranks) 4))))

(defn three-of-a-kind? [hand]
  (let [ranks (freq-ranks hand)]
    (and
      (= (apply max ranks) 3)
      (= (count ranks) 3))))

(defn four-of-a-kind? [hand]
  (let [ranks (freq-ranks hand)]
    (and
      (= (apply max ranks) 4)
      (= (count ranks) 2))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (vals (frequencies suits))]
    (= (count freq) 1)))

(defn full-house? [hand]
  (let [ranks (freq-ranks hand)]
    (= (sort ranks) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [ranks (freq-ranks hand)]
    (= (sort ranks) (seq [1 2 2]))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        min-rank (apply min sorted-ranks)]
    (or
      (= '(2 3 4 5 14) sorted-ranks)
      (= sorted-ranks (range min-rank (+ 5 min-rank))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
