(ns p-p-p-pokerface)

(defn rank [card]
  (def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (ranks fst)))
    )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rankings-sorted [hand]
  (vec (sort (map rank hand))))

(defn max-val-in-freq [hand]
  (apply max (vals (frequencies (rankings-sorted hand)))))

(defn pair? [hand]
  (== 2 (max-val-in-freq hand)))

(defn three-of-a-kind? [hand]
  (== 3 (max-val-in-freq hand)))

(defn four-of-a-kind? [hand]
  (== 4 (max-val-in-freq hand)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 5 (apply max (vals (frequencies suits))))
    ))

(defn sort-by-rank-freq [hand]
  (vec (sort (vals (frequencies (rankings-sorted hand))))))

(defn full-house? [hand]
    (= [2 3] (sort-by-rank-freq hand)))

(defn two-pairs? [hand]
  (let [sorted-ranks (sort-by-rank-freq hand)]
    (or (= [1 2 2] sorted-ranks) (= [1 4] sorted-ranks))))

(defn difference [hand]
  (- (apply max hand) (apply min hand)))

(defn straight? [hand]
  (let [ranks (rankings-sorted hand)
        replaced
        (if (and (some #(= 2 %) ranks) (some #(= 14 %) ranks))
          (vec (sort (replace {14 1} ranks)))
          ranks
          )]
    (and (= 4 (difference replaced)) (= 1(apply max (vals (frequencies replaced)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        filtered (filter (fn [check] ((first check) hand)) checkers)]
    (apply max (map second filtered))
    ))
