(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (cond (= rank \T) 10
          (= rank \J) 11
          (= rank \Q) 12
          (= rank \K) 13
          (= rank \A) 14
           )))

(defn suit [[_ suit]]
  (str suit))

(defn n-of-a-kind? [n]
  (fn [hand]
   (not (empty? (filter (fn [x] (= n x))(vals (frequencies (map rank hand))))))
    ))

(defn pair? [hand]
  ((n-of-a-kind? 2) hand))

(defn three-of-a-kind? [hand]
  ((n-of-a-kind? 3) hand))

(defn four-of-a-kind? [hand]
  ((n-of-a-kind? 4) hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     [1 2 2]))

(defn straight? [hand]
  (let [ranks (map rank hand)
        is-straight (fn [ranks]
    (let [sorted-ranks(sort ranks)
        start (apply min sorted-ranks)
        ]
       (= (range start (+ 5 start)) sorted-ranks))
     )]
    (or (is-straight ranks)
        (is-straight (replace {14 1} ranks))
    )))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))


(defn value [hand]
  (let [ high-card? (fn [x] true)
        checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
   (apply max (map second (filter (fn [[checker result]] (checker hand)) checkers)))))
