(ns p-p-p-pokerface)

(defn- count-pairs [ranks]
  (let [rank-freq-vals (vals (frequencies ranks))
        pair-ranks-vals (filter #(>= % 2) rank-freq-vals)]
    (count pair-ranks-vals)))

(defn- consecutive? [items]
  (let [items (sort items)]
    (and
      (apply < items)
      (=
       (dec (count items))
       (- (last items) (first items))))))

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
  (let [ranks (map rank hand)]
    (>= (count-pairs ranks) 1)))

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
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (= [2 3] (sort rank-freq-vals))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (>= (count-pairs ranks) 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        replaced-ranks (replace {14 1} ranks)]
    (or
      (consecutive? ranks)
      (consecutive? replaced-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
