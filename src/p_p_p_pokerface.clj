(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        letter->int {\T 10 \J 11 \Q 12 \K 13 \A 14}
        digit->int (fn [c] (Integer/valueOf (str c)))]
    (if (Character/isDigit rnk)
      (digit->int rnk)
      (letter->int rnk))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= 2 (some #{2} (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (= 3 (some #{3} freq))))

(defn four-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (= 4 (some #{4} freq))))

(defn flush? [hand]
  (let [suits (map suit hand)
        st (suit (first hand))]
    (every? #(= st %) suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))
        pair-count (count (filter #(= 2 %) freq))]
    (= 2 pair-count)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        lb (apply min sorted-ranks)
        ub (+ 1 (apply max sorted-ranks))]
    (or (= [2 3 4 5 14] sorted-ranks)
        (= (range lb ub) sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers? #{[high-card? 0] [pair? 1]
                    [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
        matcher->value (fn [[f v]] (when (f hand) v))
        values (map matcher->value checkers?)
        filtered-values (filter #(number? %) values)]
    (apply max filtered-values)))
