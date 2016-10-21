(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (str (let [[_ s] card] s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 2)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (> (apply max (vals (frequencies ranks))) 3)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (> (apply max (vals (frequencies suits))) 4)))

(defn full-house? [hand]
  (let [ranks (map rank hand) rank-frq (vals (frequencies ranks))]
    (and (= (apply max rank-frq) 3) (= (apply min rank-frq) 2))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand) rank-frq (sort (vals (frequencies ranks)))]
    (or (= [1 2 2] rank-frq) (= [1 4] rank-frq))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min-rank (apply min ranks)
        low-ranks (sort (replace {14 1} (map rank hand)))
        min-low-rank (apply min low-ranks)]
      (or (= ranks (range min-rank (+ min-rank 5))) (= low-ranks (range min-low-rank (+ min-low-rank 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0)))
