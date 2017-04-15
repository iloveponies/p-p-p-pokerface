; pair?                   2 of same rank
; three-of-a-kind?        3 of same rank
; four-of-a-kind?         4 of same rank
; full-house?             3 of same rank + another 2 of same rank
; two-pairs?              2 of same rank + another 2 of same rank
; flush?                  not sequence by rank, all same suit
; straight?               sequence by rank, not all same suit
; straight-flush?         sequence by rank, all same suit

(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rankch suitch] card]
    (.indexOf "0123456789TJQKA" (str rankch))))

(defn suit [card]
  (let [[rankch suitch] card]
    (str suitch)))

(defn all-same-suit? [hand]
  (= 1 (count (set (map suit hand)))))

(defn sequence-by-rank? [hand]
  (let [fit? (fn [ranks]
               (let [ranks (sort < ranks)]
                 (= ranks (range (first ranks) (+ (first ranks) (count ranks))))))]
    (let [ranks (map rank hand)]
      (or (fit? (replace {1 14} ranks))
          (fit? (replace {14 1} ranks))))))

(defn matching-rank-counts [hand]
  (sort <
    (remove #(= % 1)
      (map second
        (reduce (fn [counts rank] (assoc counts rank (inc (get counts rank 0))))
                {} (map rank hand))))))

(defn pair? [hand]
  (= [2] (matching-rank-counts hand)))

(defn three-of-a-kind? [hand]
  (= [3] (matching-rank-counts hand)))

(defn four-of-a-kind? [hand]
  (= [4] (matching-rank-counts hand)))

(defn flush? [hand]
  (and (all-same-suit? hand) (not (sequence-by-rank? hand))))

(defn full-house? [hand]
  (= [2 3] (matching-rank-counts hand)))

(defn two-pairs? [hand]
  (= [2 2] (matching-rank-counts hand)))

(defn straight? [hand]
  (and (sequence-by-rank? hand) (not (all-same-suit? hand))))

(defn straight-flush? [hand]
  (and (sequence-by-rank? hand) (all-same-suit? hand)))

(def valuetab
  {8 straight-flush?
   7 four-of-a-kind?
   6 full-house?
   5 flush?
   4 straight?
   3 three-of-a-kind?
   2 two-pairs?
   1 pair?
   0 (constantly true)})

(defn value [hand]
  (some (fn [val]
          (let [match? (get valuetab val)]
            (when (match? hand) val)))
        (sort > (map first valuetab))))
