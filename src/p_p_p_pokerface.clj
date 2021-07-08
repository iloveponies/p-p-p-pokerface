(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card] (let [[rank _] card]
                     (if (Character/isDigit rank)
                        (Integer/valueOf (str rank))
                        (get replacements rank))))

(defn suit [card] (let [[_ suit] card] (str suit)))

(defn ranks_of_hand [hand] (map rank hand))

(defn suits_of_hand [hand] (map suit hand))

(defn frequency_of_ranks [hand] (vals (frequencies (ranks_of_hand hand))))

(defn frequency_of_suits [hand] (keys (frequencies (suits_of_hand hand))))

(defn filter_value_from_rank_frequency [hand value] (filter (fn [v] (== value v)) (frequency_of_ranks hand)))




(defn pair? [hand] (< 0 (count (filter_value_from_rank_frequency hand 2))))

(defn three-of-a-kind? [hand] (and
                                (== 1 (count (filter_value_from_rank_frequency hand 3)))
                                (== 2 (count (filter_value_from_rank_frequency hand 1)))))

(defn four-of-a-kind? [hand] (and
                               (== 1 (count (filter_value_from_rank_frequency hand 4)))
                               (== 1 (count (filter_value_from_rank_frequency hand 1)))))

(defn flush? [hand] (== 1 (count (frequency_of_suits hand))))

(defn full-house? [hand] (and
                           (== 1 (count (filter_value_from_rank_frequency hand 3)))
                           (== 1 (count (filter_value_from_rank_frequency hand 2)))))

(defn two-pairs? [hand] (and
                          (== 2 (count (filter_value_from_rank_frequency hand 2)))
                          (== 1 (count (filter_value_from_rank_frequency hand 1)))))

(defn straight? [hand] (let [min_of_ranks_ace_14 (apply min (ranks_of_hand hand))
                             min_of_ranks_ace_one (apply min (replace {14 1} (ranks_of_hand hand)))]
                         (or
                          (=
                           (sort (ranks_of_hand hand))
                           (sort (range min_of_ranks_ace_14 (+ min_of_ranks_ace_14 5))))
                          (=
                           (sort (replace {14 1} (ranks_of_hand hand)))
                           (sort (range min_of_ranks_ace_one (+ min_of_ranks_ace_one 5)))))))

(defn straight-flush? [hand] (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand] (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
                     (apply max
                       (map
                        second
                         (filter
                          (fn [poker_hand_value_pair] ((first poker_hand_value_pair) hand))
                           checkers)))))
