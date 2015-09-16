(ns p-p-p-pokerface)

(defn rank [card]
  (let [[x] card]
    (if (Character/isDigit x)
      (Integer/valueOf (str x))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} x))))

(defn suit [card]
  (let [[_ x] card]
    (str x)))

(defn hand-rank-frequency-vals [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (contains? (set (hand-rank-frequency-vals hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (hand-rank-frequency-vals hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (hand-rank-frequency-vals hand)) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
     (contains? (set (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [sorted-frequencies (sort (hand-rank-frequency-vals hand))]
      (or
       (= sorted-frequencies [1 2 2])
       (= sorted-frequencies [1 4]))))

(defn straight? [hand]
  (let [sorted-hand-ranks (sort (map rank hand))
        sorted-hand-ranks2 (sort (replace {14 1} (map rank hand)))
        [smallest-rank-in-hand] sorted-hand-ranks]
    (or
     (= sorted-hand-ranks (range smallest-rank-in-hand (+ smallest-rank-in-hand 5)))
     (= sorted-hand-ranks2 (range 1 6)))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
