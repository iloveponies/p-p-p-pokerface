(ns p-p-p-pokerface)

(defn rank [a-card]
  (let[[r _] a-card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r))))
    
(defn suit [a-card]
  (let [[_ s] a-card]
    (str s)))

(defn rank-frequencies [a-hand]
  (vals (frequencies (map rank a-hand))))
  
(defn sorted-rank-frequencies [a-hand]
  (sort (rank-frequencies a-hand)))

(defn n-of-a-kind? [n a-hand]
  (= n (apply max (rank-frequencies a-hand))))

(def pair?            (partial n-of-a-kind? 2))
(def three-of-a-kind? (partial n-of-a-kind? 3))
(def four-of-a-kind?  (partial n-of-a-kind? 4))

(defn flush? [a-hand]
  (apply = (map suit a-hand)))

(defn full-house? [a-hand]
  (= [2 3] (sorted-rank-frequencies a-hand)))

(defn two-pairs? [a-hand]
  (let [freqs (frequencies (rank-frequencies a-hand))]
    (or (= 2 (get freqs 2))
        (= 1 (get freqs 4)))))

(defn straight? [a-hand]
  (let [sorted-ranks (sort (map rank a-hand))
        alt-sorted-ranks (sort (replace {14 1} sorted-ranks))
        rank-range (fn [ranks] (range (apply min ranks) (inc (apply max ranks))))]
    (or (= sorted-ranks (rank-range sorted-ranks))
        (= alt-sorted-ranks (rank-range alt-sorted-ranks)))))

(defn straight-flush? [a-hand]
  (and (straight? a-hand)
       (flush? a-hand)))
       
(defn high-card? [a-hand]
  true)

(defn value [a-hand]
  (let [checkers #{[high-card?       0]
                   [pair?            1]
                   [two-pairs?       2]
                   [three-of-a-kind? 3]
                   [straight?        4]
                   [flush?           5]
                   [full-house?      6] 
                   [four-of-a-kind?  7]
                   [straight-flush?  8]}]
    (apply max (map second (filter #((first %) a-hand) checkers)))))
