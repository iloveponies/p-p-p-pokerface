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

(defn ranks-of-hand [hand]
  (map rank hand))

(defn multiple-same-ranks? [hand how-many]
    (contains? (set (vals (frequencies (ranks-of-hand hand)))) how-many))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (multiple-same-ranks? hand 2))

(defn three-of-a-kind? [hand]
  (multiple-same-ranks? hand 3))

(defn four-of-a-kind? [hand]
  (multiple-same-ranks? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (vals (frequencies suits)) (seq [5]))))

(defn full-house? [hand]
    (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (ranks-of-hand hand)))) (seq [1 2 2]))))

(defn straight-with-ace? [ranks]
  (or 
    (= (sort ranks) (seq [10 11 12 13 14]))
    (= (sort (replace {14 1} ranks)) (seq [1 2 3 4 5]))))

(defn straight? [hand]
  (let [ranks (ranks-of-hand hand)
        smallest (apply min ranks)
        biggest (apply max ranks)]
    (if (== biggest 14)
      (straight-with-ace? ranks)
      (= (sort ranks) (range smallest (+ biggest 1))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hands-values (filter (fn [[value-of-hand _]] (value-of-hand hand)) checkers)]
      (apply max (map second hands-values))))
