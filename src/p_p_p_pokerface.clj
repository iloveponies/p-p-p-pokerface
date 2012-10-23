(ns p-p-p-pokerface)

(defn rank [card]
  (let [[a _] card]
    (if (Character/isDigit a)
      (Integer/valueOf (str a))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} a))))

(defn suit [card]
  (let [[_ b] card]
   (str b)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) 
      (= '(1 2 2) 
         (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranksA (sort (replace {14 1} ranks))
        rank-range (fn [r]
                     (let [eka (first r)]
                       (range eka (+ eka 5))))]
    (or (= ranks (rank-range ranks)) 
        (= ranksA (rank-range ranksA)))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? 
                  three-of-a-kind? straight? 
                  flush? full-house? four-of-a-kind? 
                  straight-flush?]
        has-value? (fn [v] 
                     ((get checkers v) hand))]
   (apply max (filter has-value? (range 9)))))