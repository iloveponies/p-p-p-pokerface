(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
  (if (Character/isDigit r)
  (Integer/valueOf (str r)) 
  (cond
    (= r \T) 10
    (= r \J) 11
    (= r \Q) 12
    (= r \K) 13
    (= r \A) 14
  ))))

(defn suit [card]
  (let [[_ s] card]
  (str s)  
 ))

(defn pair? [hand]
  (let [freq (frequencies (map rank hand))]
  (< 1 (first (vals freq)) 3 )))

(defn three-of-a-kind? [hand]
  (let [freq (frequencies (map rank hand))]
  (< 2 (first (vals freq)) 4)))

(defn four-of-a-kind? [hand]
  (let [freq (frequencies (map rank hand))]
  (< 3 (first (vals freq)) 5 )))

(defn flush? [hand]
  (let [freq (frequencies (map suit hand))]
  (== 5 (first (vals freq)))
))

(defn full-house? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
  (= freq [2 3])
  ))

(defn two-pairs? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
  (or (four-of-a-kind? hand) 
      (= freq [2 2])))  
)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)