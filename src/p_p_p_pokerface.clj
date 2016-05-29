(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank->value {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get rank->value rank))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (if (< 0 (count  (filter (fn [val]
                               (= val 2)) (vals (frequencies ranks)))))
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (< 0 (count  (filter (fn [val]
                               (= val 3)) (vals (frequencies ranks)))))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (< 0 (count  (filter (fn [val]
                               (= val 4)) (vals (frequencies ranks)))))
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (< 0 (count  (filter (fn [val]
                               (= val 5)) (vals (frequencies suits)))))
      true
      false)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (if (= 2 (count  (filter (fn [val]
                               (= val 2)) (vals (frequencies ranks)))))
      true
      false)))

(defn straight? [hand]
  (let [hand-values (map rank hand)]
    (or (let [sorted-values (sort hand-values)]
          (= (range (apply min sorted-values) (+ 1 (apply max sorted-values))) sorted-values))
        (let [sorted-values (sort (replace {14 1} hand-values))]
          (= (range (apply min sorted-values) (+ 1 (apply max sorted-values))) sorted-values)))))

(defn straight-flush? [hand]
  (and (straight? hand)
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
    :else               			 0))
