(ns p-p-p-pokerface)

(defn rank [card]
  (get {\1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14}
       (first card)))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 1 3]))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (let [sorted-suits (sort (map suit hand))]
    (= (first sorted-suits) (last sorted-suits))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 2 2]))

(defn straight? [hand]
  (let [ranks (map rank hand)
        suits (map suit hand)
        default-sorted-ranks     (sort (replace {1 14} ranks))
        alternative-sorted-ranks (sort (replace {14 1} ranks))]
   (and (>= (count (frequencies suits)) 2)
         (or (= default-sorted-ranks     (range (first default-sorted-ranks)     (inc (last default-sorted-ranks))))
             (= alternative-sorted-ranks (range (first alternative-sorted-ranks) (inc (last alternative-sorted-ranks))))))))

(defn straight-flush? [hand]
  (let [ranks (map rank hand)
        suits (map suit hand)
        default-sorted-ranks     (sort (replace {1 14} ranks))
        alternative-sorted-ranks (sort (replace {14 1} ranks))]
   (and (= (count (frequencies suits)) 1)
         (or (= default-sorted-ranks     (range (first default-sorted-ranks)     (inc (last default-sorted-ranks))))
             (= alternative-sorted-ranks (range (first alternative-sorted-ranks) (inc (last alternative-sorted-ranks))))))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [[straight-flush? 8] [four-of-a-kind? 7]
                  [full-house? 6]     [flush? 5]
                  [straight? 4]       [three-of-a-kind? 3] 
                  [two-pairs? 2]      [pair? 1]
                  [high-card? 0]]]
    (second (first (filter #((first %) hand) checkers)))))
