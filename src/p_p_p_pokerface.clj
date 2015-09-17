(ns p-p-p-pokerface)

(defn rank [card]
  (let [majors {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rank (first card)]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get majors rank))))

(defn suit [card]
  (str (last card)))

(defn ranks [hand]
  (mapv rank hand))

(defn rank-freqs [hand]
  (vals (frequencies (ranks hand))))

(defn max-rank-freq [hand]
  (apply max (rank-freqs hand)))

(defn pair? [hand]
  (> (max-rank-freq hand) 1))

(defn three-of-a-kind? [hand]
  (> (max-rank-freq hand) 2))

(defn four-of-a-kind? [hand]
  (> (max-rank-freq hand) 3))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (= (vec (sort (rank-freqs hand))) [2 3]))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  ;( let [sorted-rank (sort (set (ranks hand)))]
  ;(and (= (count (set (map suit hand))) 1)
  ;     (first sorter-rank))
  ;     ))
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
