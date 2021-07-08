(ns p-p-p-pokerface)

(defn rank [card]
  (let [rnk (get card 0)]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rnk))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (not (empty? (filter #(= 2 %) freqs)))))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (not (empty? (filter #(= 3 %) freqs)))))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (not (empty? (filter #(= 4 %) freqs)))))

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
    (not (empty? (filter #(= 5 %) freqs)))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or (= (count (filter #(= 2 %) freqs)) 2)
        (= (count (filter #(= 4 %) freqs)) 1))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        rank-range (range (apply min sorted-ranks)
                          (inc (apply max sorted-ranks)))]
    (if (and (= (take 4 sorted-ranks) (range 2 6))
             (= (get (vec sorted-ranks) 4) 14))
      true
      (= sorted-ranks rank-range))))

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
