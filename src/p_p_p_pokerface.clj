(ns p-p-p-pokerface)

(defn rank [card]
  (let [rnk (first card)]
    (if (Character/isDigit (first card))
      (Integer/valueOf (str rnk))
      ({\T 10, \J 11, \Q 12, \K 13, \A, 14} rnk))))

(defn suit [card]
  (str (second card)))

(defn freq [hand]
  (vals (frequencies (map rank hand))))

(defn max-freq [hand]
  (apply max (freq hand)))

(defn pair? [hand]
  (>= (max-freq hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-freq hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-freq hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (freq hand)) '(2 3)))

(defn two-pairs? [hand]
  (let [[a b] (sort-by > (freq hand))]
    (or
      (>= a 4)
      (and (>= a 2) (>= b 2)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        hand1 (sort ranks)
        hand2 (sort (replace {14 1} ranks))
        check (fn [hand]
                (let [start (first hand)
                      end (+ start 5)]
                  (= (range start end) hand)))
        ]
    (or (check hand1) (check hand2))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
      (map second
        (filter (fn [[match _]] (match hand)) checkers)))))
