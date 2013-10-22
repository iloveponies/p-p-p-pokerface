(ns p-p-p-pokerface)

(defn rank [[fst snd]]
  (let [pic-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (pic-cards fst))))

(defn suit [[fst snd]]
  (str snd))

(defn same-rank-counts [hand]
  (frequencies (vals (frequencies (map rank hand)))))

(defn three-of-a-kind? [hand]
  (contains? (same-rank-counts hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (same-rank-counts hand) 4))

(defn pair? [hand]
  (boolean (or (contains? (same-rank-counts hand) 2)
               (three-of-a-kind? hand)
               (four-of-a-kind? hand))))

(defn flush? [hand]
  (contains? (frequencies (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (contains? (same-rank-counts hand) 2)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (full-house? hand)
      (and (contains? (same-rank-counts hand) 2)
           (== ((same-rank-counts hand) 2) 2))))

(defn straight? [hand]
  (let [hand-vals (map rank hand)
        ace-hand-vals (replace {14 1} hand-vals)
        min-max-diff (fn [sorted-hand] (- (last sorted-hand) (first sorted-hand)))]
    (and (not (pair? hand))
         (or (== (min-max-diff (sort hand-vals)) 4)
             (== (min-max-diff (sort ace-hand-vals)) 4)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; How high are you? Yes.

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight? flush?
                  full-house? four-of-a-kind? straight-flush?]]
    ((get checkers value) hand)))

(defn closest-value [hand value]
  (if (hand-has-value? hand value) value (closest-value hand (- value 1))))

(defn value [hand]
  (closest-value hand 8)) ; recursion helper functions ftw.
