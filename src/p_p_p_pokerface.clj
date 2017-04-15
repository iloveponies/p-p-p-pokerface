(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (replacements rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  ;If there are only four different keys for values in hand, there must be a duplicate
  (let [hand_values (frequencies (map rank hand))]
    (= (count (keys hand_values)) 4)))

(defn three-of-a-kind? [hand]
  (let [hand_frequencies (frequencies (map rank hand))]
    (not (empty? (filter (fn [x] (= x 3)) (vals hand_frequencies))))))

(defn four-of-a-kind? [hand]
  (let [hand_frequencies (frequencies (map rank hand))]
    (not (empty? (filter (fn [x] (= x 4)) (vals hand_frequencies))))))

(defn flush? [hand]
  (let [hand_suites (map suit hand)]
    (= 1 (count (frequencies hand_suites)))))

(defn full-house? [hand]
  (let [hand_frequencies (frequencies (map rank hand))
        sorted_frequency_values (sort (vals hand_frequencies))]
    (= [2 3] sorted_frequency_values)))

(defn two-pairs? [hand]
  (let [sorted_hand_frequencies (sort (vals (frequencies (map rank hand))))
        frequencies_of_two (filter (fn [x] (= x 2)) sorted_hand_frequencies)]
    (or (four-of-a-kind? hand) (= 2 (count frequencies_of_two)))))

(defn straight? [hand]
  ;Check with equality between seq[low_value high_value+1] and sorted value list
  (let [sorted_ranks (sort (map rank hand))
        switched_ace (sort (replace {14 1} sorted_ranks))]
    (or
     (= switched_ace (range (apply min switched_ace) (+ 1 (apply max switched_ace))))
     (= sorted_ranks (range (apply min sorted_ranks) (+ 1 (apply max sorted_ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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



