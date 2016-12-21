(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [[rank suit] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else
      ({\T 10,
        \J 11,
        \Q 12,
        \K 13,
        \A 14} rank))))

(defn suit [card]
  (let
    [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (not (= 1 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
