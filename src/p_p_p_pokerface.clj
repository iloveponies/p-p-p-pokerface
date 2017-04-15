(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank-char))))


(defn suit [card]
  (let [[_ suit-char] card]
    (str suit-char)
    ))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))


(defn pair? [hand]
  (contains?  (set (vals (frequencies (ranks hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains?  (set (vals (frequencies (ranks hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains?  (set (vals (frequencies (ranks hand)))) 4))

(defn flush? [hand]
  (and
    (== 1 (count (set (suits hand))))
    (or
     (< hand)
     (> hand))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (let [pairs-filter (fn [count] (== count 2))]
      (== 2 (count (filter pairs-filter (vals (frequencies (ranks hand))))))
    )
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [straight?? (fn [hand] (= hand (range (apply min hand) (+ 5 (apply min hand)))))
        hand1 (sort  (ranks hand))
        hand2 (sort(replace {14 1} (ranks hand)))]
    (or
      (straight?? hand1)
      (straight?? hand2)
   )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        check (fn [[_ checker]] (checker hand))
        indexed-checker->checker (fn [_ checker] checker)
        indexed-checkers (map-indexed (fn [i checker] [i checker]) checkers)
        indexed-checker->index (fn [[i _]] i)]
    (apply max (map indexed-checker->index (filter check indexed-checkers)))))




