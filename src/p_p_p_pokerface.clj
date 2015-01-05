(ns p-p-p-pokerface)

(defn rank->int [rep rank]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get rep rank)))

(defn rank [card]
  (let [[rank _] card]
      (rank->int {\T 10 \J 11 \Q 12 \K 13 \A 14} rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))
(defn has-n-matching-cards? [n hand]
  (not (empty? (filter (fn [x] (<= n x)) (rank-frequencies hand)))))

(defn pair? [hand]
  (has-n-matching-cards? 2 hand))

(defn three-of-a-kind? [hand]
  (has-n-matching-cards? 3 hand))

(defn four-of-a-kind? [hand]
  (has-n-matching-cards? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (rank-frequencies hand))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sort (rank-frequencies hand)))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [is-straight? (fn [reps] (let [ranks (sort (replace reps (map (fn [x]
                                                                       (let [[r s] x]
                                                                         (rank->int reps r)))
                                                                     hand)))
                                         min-rank (apply min ranks)
                                         max-rank (apply max ranks)
                                         test-rank-range (range min-rank (+ max-rank 1))]
                                     (= test-rank-range ranks)))
        base-replacements {\T 10 \J 11 \Q 12 \K 13}
        low-ace_reps (assoc base-replacements \A 1)
        high-ace-reps (assoc base-replacements \A 14)]
    (or (is-straight? low-ace_reps)
        (is-straight? high-ace-reps))))


(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [chk]
                      (if ((first chk) hand)
                        (second chk)
                        0))
                    checkers))))

