(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-str (str rank)
        rank-vals {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf rank-str)
      (rank-vals rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [twos-only
        (fn [freq] (= 2 freq))]
    (= 1 (count (filter twos-only (vals (frequencies (map rank hand))))))))

(defn three-of-a-kind? [hand]
  (let [threes-only
        (fn [freq] (= 3 freq))]
    (= 1 (count (filter threes-only (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind? [hand]
  (let [fours-only
        (fn [freq] (= 4 freq))]
    (= 1 (count (filter fours-only (vals (frequencies (map rank hand))))))))

(defn flush? [hand]
  (let [fives-only
        (fn [freq] (= 5 freq))]
    (= 1 (count (filter fives-only (vals (frequencies (map suit hand))))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [twos-only
          (fn [freq] (= 2 freq))
        two-diff-pair? 
          (= 2 (count (filter twos-only (vals (frequencies (map rank hand))))))]
    (or two-diff-pair? (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        shifted-ranks (if (= 14 (last sorted-ranks))
                        (conj (take 4 sorted-ranks) 1)
                        (conj (take 4 sorted-ranks) (last sorted-ranks)))
        first-rank (first sorted-ranks)
        conseq-ranks (range first-rank (+ first-rank 5))
        shifted-first-rank (first shifted-ranks)
        shifted-conseq-ranks (range shifted-first-rank (+ shifted-first-rank 5))]
    (or (= sorted-ranks conseq-ranks)
        (= shifted-ranks conseq-ranks)
        (= shifted-ranks shifted-conseq-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers2 #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]})

(defn value [hand]
  (let [points
        (fn [item]
          (if ((first item) hand)
            (second item)
            0))]
    (apply max (mapv points checkers2))))

