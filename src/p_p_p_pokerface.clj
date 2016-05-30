(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ranking _] card]
    (if (Character/isDigit ranking)
      (Integer/valueOf (str ranking))
      (get {\A 14 \K 13 \Q 12 \J 11 \T 10} ranking))))

(rank "QS")

(defn suit [card]
  (let [[_ suite] card]
    (str suite)))


(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))



(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))



(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))



(defn flush? [hand]
  (apply = (map suit hand)))



(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))



(defn two-pairs? [hand]
  (let [ranks (set (map rank hand))]
    (or
      (and
        (== 3 (count ranks))
        (not (three-of-a-kind? hand)))
      (== 2 (count ranks)))))


(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        replaced (sort (replace {14 1} sorted-ranks))]
    (or
      (= (range (apply min ranks) (+ (apply min ranks) 5)) sorted-ranks)
      (= (range 1 6) replaced))))


(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))


(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        evals (filter (fn [check] ((first check) hand)) checkers)]
    (apply max (map second evals))))

