(ns p-p-p-pokerface)

(def coding->rank
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14
   }
  )

(defn rank [[rank-char _]]
  (if (Character/isDigit rank-char) 
    (Integer/valueOf (str rank-char))
    (coding->rank rank-char)
  ))

(defn suit [[_ ch-suit]]
  (str ch-suit))

(defn counts [hand]
  (vals (frequencies (map rank hand))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn n-of-a-kind? [n hand]
  ; hand has at least n of the same rank
  (not (empty? (filter (fn [x] (>= x n)) (counts hand)))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  ; suit equals for each card
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sort (counts hand))))

(defn two-pairs? [hand]
  ; total count of cards, which belong to pairs, has to be at least 4
  (>= (apply + (filter (fn [x] (>= x 2)) (counts hand))) 4))

(defn straight? [hand] 
  (let [naive-straight? (fn [ranks] (= '(0 1 2 3 4) (map (fn [x] (- x (apply min ranks))) (sort ranks))))
        high-ace-ranks (fn [hand] (map rank hand))
        low-ace-ranks (fn [hand] (replace {14 1} (map rank hand)))
        ]
  
    (or (naive-straight? (high-ace-ranks hand))
        (naive-straight? (low-ace-ranks hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}

        true-checkers (fn [hand] (filter (fn [ch] ((first ch) hand)) checkers))
        checker-values (fn [checkers] (map second checkers))
        ]
    (apply max (checker-values (true-checkers hand)))
  ))
