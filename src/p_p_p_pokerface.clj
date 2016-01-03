(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank_char _] card]
    (if (Character/isDigit rank_char)
      (Integer/valueOf (str rank_char))
      (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14 }]
        (replacements rank_char)))))

(defn suit [card]
  (let [[_ suit-char] card]
   (str suit-char)))

(defn rank-freq [hand]
 (vals (frequencies (map rank hand))))

(defn suit-freq [hand]
 (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (if (= (apply max (rank-freq hand)) 2) true false))

(defn three-of-a-kind? [hand]
  (if (= (apply max (rank-freq hand)) 3) true false))

(defn four-of-a-kind? [hand]
  (if (= (apply max (rank-freq hand)) 4) true false))

(defn flush? [hand]
  (if (= (apply max (suit-freq hand)) 5) true false))

(defn full-house? [hand]
  (= [2 3] (sort (rank-freq hand))))

(defn two-pairs? [hand]
   (or (= [1 4] (sort (rank-freq hand))) (= [1 2 2] (sort (rank-freq hand)))))

(defn straight? [hand]
  (let [sorted-rank-hand (sort (map rank hand))]
        (if (= (range (first sorted-rank-hand) (+ (first sorted-rank-hand) 5)) sorted-rank-hand)
              true
              (let [low-aced-sorted-rank-hand (sort (replace {14 1} sorted-rank-hand))]
                (= (range (first low-aced-sorted-rank-hand) (+ (first low-aced-sorted-rank-hand) 5)) low-aced-sorted-rank-hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}] ;
   (let [get-value
         (fn [[check? value]]
           (if (check? hand)
             value
             0  ))]
   (apply max (map get-value checkers )))))


