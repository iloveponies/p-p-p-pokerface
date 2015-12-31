(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        card-map {\T 10 \J 11 \Q 12 \K 13 \A 14 }]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (card-map fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [hand-ranks (map rank hand)
        rank-freq (frequencies (vals (frequencies hand-ranks)))]
    (contains? rank-freq 2)))

(defn three-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        rank-freq (frequencies (vals (frequencies hand-ranks)))]
    (contains? rank-freq 3)))

(defn four-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        rank-freq (frequencies (vals (frequencies hand-ranks)))]
    (contains? rank-freq 4)))

(defn flush? [hand]
  (let [hand-suits (map suit hand)
        max-suit-freq (apply max (vals (frequencies hand-suits)))
        min-suit-freq (apply min (vals (frequencies hand-suits)))]
    (= (= min-suit-freq max-suit-freq) true)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)
        rank-freq (frequencies (vals (frequencies hand-ranks)))]
    (if (four-of-a-kind? hand)
      true
      (if (contains? rank-freq 2)
        (= (get rank-freq 2) 2)
        false))))

(defn straight? [hand]
  (let [hand-ranks (map rank hand)
        low-ace-hand-ranks (replace {14 1} hand-ranks)
        sorted-first (fn [x] (first (sort x)))
        f-ha (sorted-first hand-ranks)
        f-la (sorted-first low-ace-hand-ranks)]
    (or (= (sort hand-ranks) (range f-ha (+ f-ha 5)))
        (= (sort low-ace-hand-ranks) (range f-la (+ f-la 5))))))

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
