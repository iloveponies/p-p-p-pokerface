(ns p-p-p-pokerface)

(defn rank [card]
  (let [face-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (str (get face-cards r))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn hand-to-recurring-count [hand]
  (set (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (contains? (hand-to-recurring-count hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (hand-to-recurring-count hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (hand-to-recurring-count hand) 4))

(defn flush? [hand]
  (let [key-count (count (keys (frequencies (map suit hand))))]
    (== key-count 1)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        frequency-list (vals (frequencies ranks))
        pairs-list (filter (fn [x] (== x 2)) frequency-list)]
    (== 2 (count pairs-list))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
