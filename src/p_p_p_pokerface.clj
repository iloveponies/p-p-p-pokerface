(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
       faces {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (if (Character/isDigit rnk) (Integer/valueOf (str rnk)) (faces rnk))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 1))
  
(defn three-of-a-kind? [hand]
  (= (count (filter #(= 3 %) (vals (frequencies (map rank hand))))) 1))
  
(defn four-of-a-kind? [hand]
  (= (count (filter #(= 4 %) (vals (frequencies (map rank hand))))) 1))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))
  
(defn full-house? [hand]
  (= (vals (frequencies (map rank hand))) [3 2]))

(defn two-pairs? [hand]
  (or (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 2)
      (= (count (filter #(= 4 %) (vals (frequencies (map rank hand))))) 1)))
  
(defn straight? [hand]
  (let [sorted (sort (map rank hand))
       minsorted (apply min sorted)
       repsorted (sort (replace {14 1} (map rank hand)))
       minrepsorted (apply min repsorted)]
  (or (= sorted (range minsorted (+ minsorted 5)))
    (= repsorted (range minrepsorted (+ minrepsorted 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map #(second %) (filter #(= (first %) true) (map #(seq [((first %1) hand) (second %1)]) checkers))))))

