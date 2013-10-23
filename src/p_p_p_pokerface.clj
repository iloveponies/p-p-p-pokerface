(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ordered-sequence-of-numbers-of-ranks [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn max-number-of-same-rank [hand]
  (apply max (ordered-sequence-of-numbers-of-ranks hand)))

(defn pair? [hand]
  (< 1 (max-number-of-same-rank hand)))

(defn three-of-a-kind? [hand]
  (< 2 (max-number-of-same-rank hand)))

(defn four-of-a-kind? [hand]
   (< 3 (max-number-of-same-rank hand)))

(defn max-number-of-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn flush? [hand]
  (== 5 (max-number-of-same-suit hand)))

(defn full-house? [hand]
  (= [2 3] (ordered-sequence-of-numbers-of-ranks hand)))

(defn two-pairs? [hand]
  (or (= [1 2 2] (ordered-sequence-of-numbers-of-ranks hand))
      (= [1 4] (ordered-sequence-of-numbers-of-ranks hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ace-to-one (sort (replace {14 1} (map rank hand)))]
    (if (= ranks ace-to-one)
      (= ranks (range (apply min ranks) (+ (apply min ranks) 5)))
      (or (= ranks [10 11 12 13 14])
          (= ace-to-one [1 2 3 4 5])))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
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



