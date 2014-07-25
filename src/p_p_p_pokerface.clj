(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (def sorted-hand (sort hand))
  (= (vals (frequencies(map rank sorted-hand))) '(3 2)))

(defn two-pairs? [hand]
  (def sorted-hand (sort hand))
  (= (vals (frequencies(map rank sorted-hand))) '(2 2 1)))

(defn straight? [hand]
  (defn sorted-hand [rank]
    (let [replaced
      (if (and (= (apply min rank) 2)
        (= (apply max rank) 14))
        (replace {14 1} rank)
        rank)]
      (sort replaced)))
  (let [sorted (sorted-hand (map rank hand))
        min (apply min sorted)
        max (apply max sorted)
        check (range min (+ max 1))]
    (= sorted check)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value-table [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
  checked (filter (fn [[rank _]] (rank hand)) checkers)
  vals (map second checked)]
  (apply max vals)))
