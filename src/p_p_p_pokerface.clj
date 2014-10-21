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
  nil)

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
       minsorted (apply min sorted)
       repsorted (sort (replace {14 1} (map rank hand)))
       minrepsorted (apply min repsorted)]
  (or (= sorted (range minsorted (+ minsorted 5)))
    (= repsorted (range minrepsorted (+ minrepsorted 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
