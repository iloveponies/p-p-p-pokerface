(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [a-card]
  (let [[val _] (card a-card)]
    (if (Character/isDigit val)
      (Integer/valueOf (str val))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} val))))

(defn suit [a-card]
  (let [[_ sut] (card a-card)]
    (str sut)))

(defn- rank-frq [a-hand]
  (vals (frequencies (map #(rank %) a-hand))))

(defn- rank-values [a-hand]
  (keys (frequencies (map #(rank %) a-hand))))

(defn- suit-frq [a-hand]
  (vals (frequencies (map #(suit %) a-hand))))

(def amax (partial apply max))

(defn pair? [a-hand]
  (> (amax (rank-frq a-hand)) 1))

(defn three-of-a-kind? [a-hand]
  (> (amax (rank-frq a-hand)) 2))

(defn four-of-a-kind? [a-hand]
  (> (amax (rank-frq a-hand)) 3))

(defn flush? [a-hand]
  (= 5 (amax (suit-frq a-hand))))

(defn full-house? [a-hand]
  (and (not (nil? (some #(= % 2) (rank-frq a-hand))))
       (not (nil? (some #(= % 3) (rank-frq a-hand))))))

(defn two-pairs? [a-hand]
  (or (four-of-a-kind? a-hand)
      (= 2 (count (filter #(= % 2) (rank-frq a-hand))))))

(defn straight? [a-hand]
  (let [values (sort (rank-values a-hand))
        lowest-rank (first values)
        highest-rank (last values)]
    (if (= highest-rank 14)
      (and (= (count (rank-frq a-hand)) 5)
           (or (= (range 2 (inc (nth values 3))) [2 3 4 5])
               (= (range lowest-rank (inc highest-rank)) values)))
      (and (= (count (rank-frq a-hand)) 5)
           (= (range lowest-rank (inc highest-rank)) values)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [a-hand]
  true)

(defn value [a-hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        valid-hands (map #(when ((first %) (hand a-hand)) (second %)) checkers)
        hand-values (filter #(not (nil? %)) valid-hands)]
    (amax hand-values)))
