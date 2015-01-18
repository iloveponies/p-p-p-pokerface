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
nil)

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

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
