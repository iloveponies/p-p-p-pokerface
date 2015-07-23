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

;; put the cards in ascending order
;; is the difference between max and min card 4
;; is each card less than the next card

(defn straight-helper [xs]
  (let [diff-is-4 (== 4 (- (apply max xs) (apply min xs)))
        each-different (apply < xs)]
    (and diff-is-4 each-different)))

(defn straight? [hand]
  (let [ranks-14 (sort (map rank hand))
        ranks-1 (sort (replace {14 1} ranks-14))]
    (or (straight-helper ranks-14) (straight-helper ranks-1))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        truthies (map (fn [[f v]] (if (f hand) v 0)) checkers)]
    ; now let's do it
    (apply max truthies)))

