(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(rank "TS")

(defn get-rank-frequencies [hand]
  (vals (frequencies
         (map (fn [card] (rank card)) hand))))

(defn get-suit-frequencies [hand]
  (vals (frequencies
         (map (fn [card] (suit card)) hand))))


(defn pattern-occured? [hand pattern times]
  (== (count (filter (fn [combi] (== combi pattern)) (get-rank-frequencies hand))) times))

(defn pair? [hand]
  (pattern-occured? hand 2 1))

(defn three-of-a-kind? [hand]
  (pattern-occured? hand 3 1))

(defn four-of-a-kind? [hand]
  (pattern-occured? hand 4 1))

(defn flush? [hand]
  (== (count (get-suit-frequencies hand)) 1))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))


(defn two-pairs? [hand]
  (pattern-occured? hand 2 2))

(defn straight? [hand]
  (and (or  (== 4 (-
                   (apply max (map (fn [card] (rank card)) hand))
                   (apply min (map (fn [card] (rank card)) hand))))
            (== 12 (-
                    (apply max (map (fn [card] (rank card)) hand))
                    (apply min (map (fn [card] (rank card)) hand)))))
       (== (count (get-rank-frequencies hand)) 5)))



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
