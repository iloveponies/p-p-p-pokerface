(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card
        ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn max-rank
  ;util
  [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (< 1 (max-rank hand)))

(defn three-of-a-kind? [hand]
  (< 2 (max-rank hand)))

(defn four-of-a-kind? [hand]
  (< 3 (max-rank hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn sorted-ranks
  ;util
  [hand]
  (sort (vals (frequencies (map rank hand)))))


(defn full-house? [hand]
  (= [2 3] (sorted-ranks hand)))

(defn two-pairs? [hand]
  (or
    (= [1 4] (sorted-ranks hand))
    (= [1 2 2] (sorted-ranks hand))))

(defn maybe-straight?
  ;util
  [ranks]
  (let [min-rank (apply min ranks)
        max-rank (apply max ranks)
        dif (- max-rank min-rank)
        count (count (set ranks))]
    (and (= 4 dif) (= 5 count))))

(defn straight? [hand]
  (or
     (maybe-straight? (map rank hand))
     (maybe-straight? (replace {14 1} (map rank hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map second
             (filter (fn [x] ((first x) hand)) checkers)))))

