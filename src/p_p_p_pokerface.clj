(ns p-p-p-pokerface)

(defn rank [card]
  (let [ch (get card 0)]
    (if (Character/isDigit ch)
      (Integer/valueOf (str ch))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} ch))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
    true
    (= 2 (count (filter (fn [x] (= x 2))(vals (frequencies (map rank hand))))))) )

(defn straight? [hand]
  (let [h1 (sort (map rank hand))
        h2 (sort (replace {14 1} (map rank hand)))]
    (or (= h1 (range (first h1) (+ 5 (first h1))))
        (= h2 (range (first h2) (+ 5 (first h2)))))
    )
  )

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn orig-value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-fn (fn [pair]
                   (if ((first pair) hand)
                     (second pair)
                     0))]
    (apply max (map check-fn checkers))))
