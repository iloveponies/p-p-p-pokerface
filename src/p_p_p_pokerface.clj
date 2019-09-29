(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freq (frequencies (map rank hand))]
    (== 4 (apply * (vals freq)))))

(defn straight? [hand]
  (let [hand-ranks       (map rank hand)
        sorted-hand      (sort hand-ranks)
        low-card         (apply min hand-ranks)
        high-str8-range  (range low-card (+ low-card 5))
        low-str8-range   (sort (conj (range 2 6) 14))]
        (or (= sorted-hand low-str8-range) (= sorted-hand high-str8-range))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max(map second (filter (fn [checker] ((first checker) hand)) checkers)))))
