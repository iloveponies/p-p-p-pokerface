(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (replacements rnk))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if (some #(== 2 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (some #(== 3 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (some #(== 4 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn flush? [hand]
  (if (some #(== 5 %) (vals (frequencies (map suit hand))))
    true
    false))

(defn full-house? [hand]
  (=
   '(2 3)
   (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [rank-freq (sort (vals (frequencies (map rank hand))))]
    (or
     (= '(1 2 2) rank-freq)
     (= '(1 4) rank-freq))))

(defn straight? [hand]
  (let [ranks (map rank hand) inv-ranks (replace {14 1} ranks)
        low-card (apply min ranks) inv-low-card (apply min inv-ranks)]
    (or
     (= (range low-card (+ low-card 5)) (sort ranks))
     (= (range inv-low-card (+ inv-low-card 5)) (sort inv-ranks)))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [values (map #(if ((first %) hand)
                         (second %)
                         0) 
                      checkers)]
      (apply max values))))











