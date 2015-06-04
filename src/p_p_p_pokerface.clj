(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        char-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (char-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (boolean (some (fn [x] (>= x 2)) rank-freq-vals))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (boolean (some (fn [x] (>= x 3)) rank-freq-vals))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (boolean (some (fn [x] (>= x 4)) rank-freq-vals))))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freq-vals (vals (frequencies suits))]
    (= 1 (count suit-freq-vals))))

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
