(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get values fst))))

(defn suit [card]
  (let [[_ snd] card]
   (str snd)))

(defn check-rank-match [hand]
  ( let [ranks (map rank hand)
        amounts (vals (frequencies ranks))
        fin-pair(apply max amounts)]
    fin-pair
   ))

(defn pair? [hand]
    (not (== (check-rank-match hand) 1)))

(defn three-of-a-kind? [hand]
  (>= (check-rank-match hand) 3))

(defn four-of-a-kind? [hand]
  (>= (check-rank-match hand) 4))

(defn flush? [hand]
  ( let [suits (map suit hand)
         amount (vals (frequencies suits))
         fin-flush(apply max amount)]
    (== fin-flush 5)
    ))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        amount (vals (frequencies ranks))
        max-amount (apply max amount)
        min-amount (apply min amount)]
    (and (== min-amount 2)
         (== max-amount 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        amount (vals (frequencies ranks))
        sorted-amount (sort amount)]
    (not (== (second sorted-amount) 1))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        min-rank (first sorted-ranks)
        sorted-ranks-aone (sort (replace {14 1} ranks))
        straight-from-min (range min-rank (+ min-rank 5))]
    (or (= sorted-ranks straight-from-min)
        (= sorted-ranks-aone (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))


(defn value [hand]
  (let [poker-values [(if (pair? hand) 1 0)
                     (if (two-pairs? hand) 2 0)
                     (if (three-of-a-kind? hand) 3 0)
                     (if (straight? hand) 4 0)
                     (if (flush? hand) 5 0)
                     (if (full-house? hand) 6 0)
                     (if (four-of-a-kind? hand) 7 0)
                     (if (straight-flush? hand) 8 0)]]
    (apply max poker-values)))


