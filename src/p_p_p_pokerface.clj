(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
         replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) 
                                 (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2) true false))

(defn three-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 3) true false))

(defn four-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 4) true false))

(defn flush? [hand]
  (if (== (apply max (vals (frequencies (map suit hand)))) 5) true false))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand)) true false))

(defn two-pairs? [hand]
  (let [pairs (sort (vals (frequencies (map rank hand))))]
    (if (or (four-of-a-kind? hand) (= [1 2 2] pairs)) true false)))

(defn straight? [hand]
  (let [suits (apply max (vals (frequencies (map suit hand))))
        pairs (sort (map rank hand))]
        (if (and (<= suits 4) 
            (or (= [2 3 4 5 6] pairs) (= [3 4 5 6 7] pairs) (= [4 5 6 7 8] pairs) (= [5 6 7 8 9] pairs)
                (= [6 7 8 9 10] pairs) (= [7 8 9 10 11] pairs) (= [8 9 10 11 12] pairs) (= [9 10 11 12 13] pairs)
                (= [10 11 12 13 14] pairs) (= [2 3 4 5 14] pairs))) true false)))

(defn straight-flush? [hand]
  (let [pairs (sort (map rank hand))]
    (if (and (flush? hand) 
             (or (= [2 3 4 5 6] pairs) (= [3 4 5 6 7] pairs) (= [4 5 6 7 8] pairs) (= [5 6 7 8 9] pairs)
                (= [6 7 8 9 10] pairs) (= [7 8 9 10 11] pairs) (= [8 9 10 11 12] pairs) (= [9 10 11 12 13] pairs)
                (= [10 11 12 13 14] pairs) (= [2 3 4 5 14] pairs))) true false)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [values {"high-card" 0, "pair" 1, "two-pairs" 2, "three-of-a-kind" 3,
                "straight" 4, "flush" 5, "full-house" 6, "four-of-a-kind" 7, "straight-flush" 8}]
  (cond 
    (straight-flush? hand) (values "straight-flush")
    (four-of-a-kind? hand) (values "four-of-a-kind")
    (full-house? hand) (values "full-house")
    (flush? hand) (values "flush")
    (straight? hand) (values "straight")
    (three-of-a-kind? hand) (values "three-of-a-kind")
    (two-pairs? hand) (values "two-pairs")
    (pair? hand) (values "pair")
    (high-card? hand) (values "high-card"))))