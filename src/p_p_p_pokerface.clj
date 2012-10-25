(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [ranks (count (vals (frequencies (map rank hand))))]
    (and (== ranks 2) (not (four-of-a-kind? hand)))))

(defn two-pairs? [hand]
  (or (= '(1 2 2) (sort (vals (frequencies (map rank hand)))))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks))
        rankrange (fn [rr]
                    (let [lowest (first rr)]
                      (range lowest (+ lowest 5))))]
    (or (= ranks (rankrange ranks)) 
        (= ranks2 (rankrange ranks2)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
       hand-has-value? (fn [v] ((get checkers v) hand))]
    (apply max (filter hand-has-value? (range 0 9)))))
