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
      (= freq [1 2 2])))  
)

(defn sequentialorder? [hand] 
 (let [sorted (sort hand)]
 (= sorted (range (first sorted) (+ (first sorted) 5)))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
   (or (sequentialorder? (replace {14 1} ranks)) 
    (sequentialorder? ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
 true)

(defn hand-has-value? [hand value] 
(let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
               flush? full-house? four-of-a-kind? straight-flush?]]
  ((get checkers value) hand) ))

(defn value [hand]
  (cond 
  (hand-has-value? hand 8) 8  
  (hand-has-value? hand 7) 7  
  (hand-has-value? hand 6) 6     
  (hand-has-value? hand 5) 5   
  (hand-has-value? hand 4) 4  
  (hand-has-value? hand 3) 3   
  (hand-has-value? hand 2) 2  
  (hand-has-value? hand 1) 1
  (hand-has-value? hand 0) 0
  ))

