(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    ({\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7
      \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14}  fst)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn rank-frequencies [hand]
  (frequencies (ranks hand)))

(defn has-tuple [count hand]
  (not (= nil (some #{count} (vals (rank-frequencies hand))))))

(defn pair? [hand]
  (has-tuple 2 hand))

(defn three-of-a-kind? [hand]
  (has-tuple 3 hand))

(defn four-of-a-kind? [hand]
  (has-tuple 4 hand))

(defn flush? [hand]
  (= 1 (count (set (suits hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (rank-frequencies hand)))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (rank-frequencies hand)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (ranks hand))
        first-rank (first sorted-ranks)
        shifted-ranks (map (fn [r] (- r first-rank)) sorted-ranks)]
    (or (= [0 1 2 3 4] shifted-ranks)
        (= [0 1 2 3 12] shifted-ranks))))

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
