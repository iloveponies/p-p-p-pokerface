(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [different-ranks 
        (count (vals (frequencies (map rank hand))))]
    (and (== different-ranks 2) (not (four-of-a-kind? hand)))))

(defn two-pairs? [hand]
  (or
	(four-of-a-kind? hand)
    (and
      (== 3 (count (vals (frequencies (map rank hand)))))
      (pair? hand))))

(defn straight? [hand]
  (let [ranks (replace {1 14} (map rank hand))
        sorted-ranks (sort ranks)
        min-rank (apply min sorted-ranks)
        straight (range min-rank (+ min-rank 5))]
    (or 
     (= straight sorted-ranks)
     (= [2 3 4 5 14] sorted-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]
        function (get checkers value)]
    (function hand)))

(defn value [hand]
  (let [values [8 7 6 5 4 3 2 1 0]]
    (apply max (filter (fn has-value [number] (hand-has-value? hand number)) values))))