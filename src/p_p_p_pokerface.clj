(ns p-p-p-pokerface)



(defn rank [card]
  (let [[raw-rank _] card
        map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit raw-rank)
      (let [rank (Integer/valueOf (str raw-rank))]
        (if (<= 1 rank 10)
          rank
          nil))
      (map raw-rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 2)))


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= (apply max freqs) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (sort freqs) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or (= freqs (seq [1 4])) (= freqs (seq [1 2 2])))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        smallest (apply min sorted-ranks)
        biggest (apply max sorted-ranks)
        is-straight (range smallest (+ biggest 1))]
    (if (= biggest 14)
      (let [ace-card (filter (fn [x] (= (rank x) 14)) hand)
            ace-card-suit (suit (first ace-card))
            ace-fixed-hand (sort (replace {(first ace-card) (str 1 ace-card-suit)} hand))]
        ;ace-fixed-hand))))
        (or (= sorted-ranks is-straight) (straight? ace-fixed-hand)))
      (= sorted-ranks is-straight))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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























