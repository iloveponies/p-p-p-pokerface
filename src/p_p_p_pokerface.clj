(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14 })

(defn rank [card]
  (let [[rank _] card] 
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand))) ]
    (= (sort rank-freqs) '(2 3))))

(defn two-pairs? [hand]
  (let [rank-freqs (sort (vals (frequencies (map rank hand)))) 
        two-pairs-freqs '(1 2 2)
        four-in-hand-freqs '(1 4)]
    (or
     (= rank-freqs two-pairs-freqs)
     (= rank-freqs four-in-hand-freqs))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)]
    (or
     (= (sort ranks) (range min-rank (+ 5 min-rank)))
     (= (sort (replace {14 1} ranks)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] 
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max 
           (map second 
                (filter (fn [[matcher value]] (matcher hand)) checkers)))))

