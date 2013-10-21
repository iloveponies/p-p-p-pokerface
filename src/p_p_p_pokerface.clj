(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        conversions {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else                    (conversions rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn high-card? [hand] true)

(defn pair? [hand]
  (if (in? (vals (frequencies (map rank hand))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (in? (vals (frequencies (map rank hand))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (in? (vals (frequencies (map rank hand))) 4)
    true
    false))

(defn flush? [hand]
  (= (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (if (and (in? (vals (frequencies (map rank hand))) 2) (in? (vals (frequencies (map rank hand))) 3))
    true
    false))

(defn two-pairs? [hand]
  (= (count (filter (fn [n] (= n 2)) (vals (frequencies (map rank hand))))), 2))

(defn straight? [hand]
  (let [a (sort (map rank hand))
        b (sort (replace { 14 1 } a))
        a_min (first a)
        b_min (first b)]
    (or (= (range a_min (+ a_min 5)) a) (= (range b_min (+ b_min 5)) b) )))

(defn straight-flush? [hand]
  (and (= (count (vals (frequencies (map suit hand)))) 1) (straight? hand)))

(defn hand-has-type? [hand checker-value]
  ((first checker-value) hand))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (second (first (sort-by second > (filter #(hand-has-type? hand %) checkers))))))
