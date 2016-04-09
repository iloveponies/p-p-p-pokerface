(ns p-p-p-pokerface)

(defn rank [card]
  (let [[R _] card
        val {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if(Character/isDigit R)
     (Integer/valueOf (str R))
     (get val R))))

(defn suit [card]
  (let [[_ S] card]
    (str S)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (== (apply * (vals (frequencies (map rank hand)))) 6))

(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (and
      (contains? freq 2)
      (== (get freq 2) 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        corr (if(== (apply min ranks) 2) (replace {14 1} ranks) ranks)
        fin (map (fn [x] (- x (apply min corr))) (sort corr))]
    (= fin (range 5))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.
  
(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        has (filter (fn [[f _]] (f hand)) checkers)
        val (map second has)]
    (apply max val)))
