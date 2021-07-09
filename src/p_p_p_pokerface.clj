(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        crd-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get crd-map fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
    (= 1 (get (frequencies (vals (frequencies (map rank hand)))) 4))))

(defn straight? [hand]
  (let [high-ranks (sort (map rank hand))
        high-smallest (first high-ranks)
        low-ranks (sort (replace {14 1} high-ranks))
        low-smallest (first low-ranks)]
    (or
      (= high-ranks (range high-smallest (+ high-smallest 5)))
      (= low-ranks (range low-smallest (+ low-smallest 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
