(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ran _] card]
    (if (Character/isDigit ran) 
      (Integer/valueOf (str ran))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} ran))))

(defn suit [card]
  (let [[_ sui] card]
   (str sui)))

(defn pair? [hand] 
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (== 2 (apply min (vals (frequencies (map rank hand))))) 
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
      (= [1 4] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  ())

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  ())