(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank 
  "return the rank of parameter card"
  [card]
  (let [rank (first card)]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank)
    )))

(defn suit 
  "return the suit of parameter card"
  [card]
  (str (second card)))

(defn pair? 
  "does parameter hand contain a pair?"
  [hand]
  (let [comparison (fn [x] (= x 2))]
    (not (empty? (filter comparison (vals (frequencies (map rank hand))))))))

(defn three-of-a-kind? 
  "does parameter hand contain three of a kind?"
  [hand]
  (let [comparison (fn [x] (= x 3))]
    (not (empty? (filter comparison (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind?   
  "does parameter hand contain four of a kind?"
  [hand]
  (let [comparison (fn [x] (= x 4))]
    (not (empty? (filter comparison (vals (frequencies (map rank hand))))))))

(defn flush?
  "is parameter hand a flush?"
  [hand]
  (let [comparison (fn [x] (= x 5))]
    (not (empty? (filter comparison (vals (frequencies (map suit hand))))))))

(defn full-house? 
  "is parameter hand a full house?"
  [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs?
  "does parameter hand contain two pairs?" 
  [hand]
  (or (four-of-a-kind? hand)
      (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))))


;
; helper functions for straight?
;
(defn is-true? [x] (= true x))

(defn is-ace? [card] (= \A (first card)))

(defn contains-ace? [hand] (not (empty? (filter is-true? (map is-ace? hand)))))

(defn min-val [hand] (apply min (map rank hand)))

(defn check-helper
  [hand]
  (= 
    (range (min-val hand) (+ 5 (min-val hand))) 
    (sort (map rank hand))))
    

(defn straight?
  "is hand a straight?"
  [hand]
  (if (contains-ace? hand)
    (if (= (min-val hand) 2)
      (= 
        (seq [1 2 3 4 5]) 
        (sort (replace {14 1} (map rank hand))))
      (check-helper hand))
    (check-helper hand)))


(defn straight-flush? 
  "is hand a straight flush?"
  [hand]
  (and (flush? hand) (straight? hand)))

; helper functions for value

(defn high-card? [hand] true)

(defn hand-has-value?
  "check if hand worth equals parameter value"
  [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
    (get 
      (vec (map 
             (fn [func] (apply func (set (list hand)))) 
             checkers)) 
    value)))

(defn 
  value 
  "return the numeric value of parameter hand"
  [hand]
  (cond
    (hand-has-value? hand 8) 8
    (hand-has-value? hand 7) 7
    (hand-has-value? hand 6) 6
    (hand-has-value? hand 5) 5
    (hand-has-value? hand 4) 4
    (hand-has-value? hand 3) 3
    (hand-has-value? hand 2) 2
    (hand-has-value? hand 1) 1
    :else 0))
