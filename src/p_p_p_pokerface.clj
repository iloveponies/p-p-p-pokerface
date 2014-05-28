(ns p-p-p-pokerface)

(defn rank [card]
  (let [number (get card 0)
        figures {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit number)
      (Integer/valueOf (str number))
      (figures number))
    ))

(defn suit [card]
 (str (get card 1)))


(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
  (or (= values (seq [1 2 2])) (= values (seq [2 3])) (= values (seq [1 4])))))

(defn straight? [hand]
  (let [values (set(sort (map rank hand)))]
    (if (pair? hand)
      false
      (if (contains? values 14)
        (or (= values #{2 3 4 5 14}) (= values #{10 11 12 13 14}))
        (= values (set (range (apply min values) (+ (apply max values) 1))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.


(defn value [hand]
  (let [values #{}]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   (high-card? hand) 0
   )))






