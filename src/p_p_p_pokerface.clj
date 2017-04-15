(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10 \J 11 \Q 12
       \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [rank-seq (map rank hand)
        freq-rank (frequencies rank-seq)]
    (contains? (set (vals freq-rank)) 2)))

(defn three-of-a-kind? [hand]
  (let [rank-seq (map rank hand)
        freq-rank (frequencies rank-seq)]
    (contains? (set (vals freq-rank)) 3)))

(defn four-of-a-kind? [hand]
  (let [rank-seq (map rank hand)
        freq-rank (frequencies rank-seq)]
    (contains? (set (vals freq-rank)) 4)))

(defn flush? [hand]
  (let [suits-in-hand (map suit hand)
        num-of-suits (count (set suits-in-hand))]
    (if (= num-of-suits 1)
      true
      false)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rank-seq (map rank hand)
      freq-rank (frequencies rank-seq)
      vals-freq (vals freq-rank)
      freq-vals (frequencies vals-freq)
      vals-fv (vals freq-vals)]
  (if (or (and (contains? (set vals-fv) 2)
               (contains? (set vals-freq) 2))
          (contains? (set vals-freq) 4))
    true
    false)))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        hand-min-rank (apply min sorted-hand)
        would-be-flush-hand-seq (range hand-min-rank
                                       (+ 5 hand-min-rank))]
    (if (= sorted-hand would-be-flush-hand-seq)
      true
      (let [new-hand (replace {14 1} (map rank hand))
           sorted-new-hand (sort new-hand)
           new-hand-min-rank (apply min sorted-new-hand)
           new-would-be-flush-hand-seq (range new-hand-min-rank
                                       (+ 5 new-hand-min-rank))]
        (if (= sorted-new-hand new-would-be-flush-hand-seq)
          true
          false)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [x] true)
        index [0 1 2 3 4 5 6 7 8]
        checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        hand-map (vec (map {true 1, false 0} (map (fn [x] ((checkers x) hand)) index)))]

    (apply max (filter (fn [x] (= 1 (get hand-map x))) index))))



