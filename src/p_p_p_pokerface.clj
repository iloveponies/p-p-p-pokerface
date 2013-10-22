(ns p-p-p-pokerface)

(def high-card-ranks
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (high-card-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;----------------------------------------------;

(defn rank-sequence [hand]
  (sort (map rank hand)))

(defn rank-frequence [hand]
  (sort(vals (frequencies (rank-sequence hand)))))

(defn suit-frequence [hand]
  (vals (frequencies (map suit hand))))

(defn has-frequency? [rank hand]
  (boolean (some #{rank} (rank-frequence hand))))

(defn ace-replaced [hand]
  (into [1] (remove (set [14]) (rank-sequence hand))))

(defn check [hand])

;----------------------------------------------;

(defn high-card? [hand]
  true)

(defn pair? [hand]
   (has-frequency? 2 hand))

(defn three-of-a-kind? [hand]
   (has-frequency? 3 hand))

(defn four-of-a-kind? [hand]
   (has-frequency? 4 hand))

(defn flush? [hand]
  (boolean (some #{5} (suit-frequence hand))))

(defn full-house? [hand]
   (and (has-frequency? 2 hand) (has-frequency? 3 hand)))

(defn two-pairs? [hand]
   (or (= (rank-frequence hand) [1 2 2])
       (four-of-a-kind? hand)))

(defn not-low-straight? [hand]
  (let [rank-seq (rank-sequence hand)
        min-rank (apply min rank-seq)
        straight (range min-rank (+ min-rank 5))]
    (= rank-seq straight)))

(defn straight? [hand]
  (or (not-low-straight? hand)
      (= (ace-replaced hand) [1 2 3 4 5])))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
   (apply max (map-indexed (fn [i check] (if (check hand) i 0)) checkers))))

