(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]

    (cond
     (Character/isDigit rnk) (Integer/valueOf (str rnk))
     :else (replacements rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-frequency-list [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequency-list [hand]
  (vals (frequencies (map suit hand))))

(defn n-of-a-kind [hand n]
  (<= n
     (apply max (rank-frequency-list hand))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (== 5
      (apply max (suit-frequency-list hand))))

(defn full-house? [hand]
  (= (range 2 4)
     (sort (rank-frequency-list hand))))

(defn two-pairs? [hand]
  (let [sorted-freqs (sort > (rank-frequency-list hand))]
    (or
     (four-of-a-kind? hand)
     (and
      (= 2 (first sorted-freqs))
      (= 2 (first (rest sorted-freqs)))))))

(defn consecutive? [rank-list]
  (= (range (apply min rank-list) (inc (apply max rank-list)))
     (sort rank-list)))

(defn straight? [hand]
  (let
    [rank-list (map rank hand)
     rank-list-alternate (replace {14 1} rank-list)]
    (or
     (consecutive? rank-list)
     (consecutive? rank-list-alternate))))

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
              (filter (fn [checker] ((first checker) hand)) checkers)))))
