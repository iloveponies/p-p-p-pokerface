(ns p-p-p-pokerface)

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn rank-hand [hand]
  (map rank hand))

; private
(defn rank-count [hand]
  (frequencies (rank-hand hand)))

; private
(defn rank-counts [hand]
  (vals (rank-count hand)))

; private
(defn max-rank-counts [hand]
  (apply max (rank-counts hand)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= 2 (max-rank-counts hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-rank-counts hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-rank-counts hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [3 2] (rank-counts hand)))

(defn two-pairs? [hand]
  (= [2 2 1] (rank-counts hand)))

(defn straight-ranked-hand? [ranked-hand]
  (let [sorted-hand (sort ranked-hand)
        min-card (first sorted-hand)]
    (=
     (range min-card (+ min-card 5))
     sorted-hand)))

(defn straight? [hand] 
  (let [ranked-hand (rank-hand hand)]
    (or (straight-ranked-hand? ranked-hand)
        (straight-ranked-hand? (replace {14 1} ranked-hand)))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  nil)
