(ns p-p-p-pokerface)

(defn rank [[fst _]]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst)) (ranks fst))))

(defn suit [[_ snd]]
  (str snd))

(defn rank-evaluation [hand]
  (set (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (contains? (rank-evaluation hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (rank-evaluation hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (rank-evaluation hand) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (and (pair? hand)
           (and (== (count (frequencies (map rank hand))) 3)
                (contains? (rank-evaluation hand) 1)))))

(defn straight? [hand]
  (let [hand-of-ranks (map rank hand)
        high-ace-hand (sort hand-of-ranks)
        low-ace-hand (sort (replace {14 1} hand-of-ranks))
        hand-in-range?
        (fn [x] (= x (range (apply min x) (+ (apply max x) 1))))]
    (or (hand-in-range? high-ace-hand) (hand-in-range? low-ace-hand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}  
        matching-hands (filter (fn [x] ((first x) hand)) checkers)
        hand-values (map second matching-hands)]
  (apply max hand-values)))

