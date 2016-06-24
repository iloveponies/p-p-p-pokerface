(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-subs {\T 10, \J 11 \Q 12 \K 13 \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get rank-subs rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (if (< (apply max (vals (frequencies ranks))) 2)
      false
      true)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (< (apply max (vals (frequencies ranks))) 3)
      false
      true)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (< (apply max (vals (frequencies ranks))) 4)
      false
      true)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (= (apply max (vals (frequencies suits))) 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (= (sort freqs) '(2 3))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (if (or
          (= (sort freqs) '(2 3))
          (= (sort freqs) '(1 2 2))
          (= (sort freqs) '(1 4)))
      true
      false)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace-ranks (replace {14 1} ranks)
        straight (fn [x] (range x (+ x 5)))]
    (if (or
          (= (sort ranks) (straight (apply min ranks)))
          (= (sort low-ace-ranks) (straight (apply min low-ace-ranks))))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

