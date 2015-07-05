(ns p-p-p-pokerface)

(defn rank [card]
  (let [letter (first card)
        ranks-by-letter {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit letter)
      (Integer/valueOf (str letter))
      (get ranks-by-letter letter))))

(defn suit [card]
  (str (second card)))

(defn- n-of-a-kind? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs?
  "This implementation will *not* accept 4-of-a-kind as two pairs."
  [hand]
  (= [1 2 2]
     (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lowest-rank (apply min ranks)
        has-ace (contains? (set ranks) 14)]
    (or (= (range lowest-rank (+ lowest-rank 5)) ranks)
        (and has-ace (= [2 3 4 5 14] ranks)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn- high-card? "bonk" [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        tests (map first checkers)
        scores (map second checkers)]
    ; TODO
    0))
