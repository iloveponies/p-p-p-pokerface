(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-map rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- max-same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 2 max-same-rank)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 3 max-same-rank)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 4 max-same-rank)
      true
      false)))

(defn max-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn flush? [hand]
  (let [max-same-suit (max-same-suit hand)]
    (if (= (count hand) max-same-suit)
      true
      false)))

(defn full-house? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (if (= (seq [2 3]) (sort rank-frequencies))
      true
      false)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
