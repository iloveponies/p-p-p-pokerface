(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rankChar _] card]
    (if (Character/isDigit rankChar) 
      (Integer/valueOf (str rankChar))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rankChar))))

(defn suit [card]
  (let [[_ suitChar] card]
  (str suitChar)))

(defn pair? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freqs) 4)))

(defn flush? [hand]
   (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (and (contains? (set rank-freqs) 2) 
         (contains? (set rank-freqs) 3))))

(defn two-pairs? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (or (= (sort rank-freqs) [1 2 2])
        (full-house? hand) 
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks-high (map rank hand) 
        ranks-low (replace {14 1} ranks-high)
        get-max (fn [ranks] (+ 1 (apply max ranks)))]
       (or (= (sort ranks-high) (range (apply min ranks-high) (get-max ranks-high)))
           (= (sort ranks-low) (range (apply min ranks-low) (get-max ranks-low))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
