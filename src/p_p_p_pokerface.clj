(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\A 14, \T 10, \J 11, \Q 12, \K 13}
        [fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get ranks fst))))

(defn ranks [hand]
  (map rank hand))

(defn is-two? [r] (= 2 r))

(defn is-three? [r] (= 3 r))

(defn rank-freqs [hand]
  (vals (frequencies (ranks hand))))

(defn rank-count [p freqs]
  (count (filter p freqs)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn suits [card]
  (map suit card))

(defn pair? [hand]
  (= 1 (rank-count is-two? (rank-freqs hand))))

(defn three-of-a-kind? [hand]
  (= 1 (rank-count is-three? (rank-freqs hand))))

(defn four-of-a-kind? [hand]
  (= 1 (rank-count (fn [r] (= 4 r)) (rank-freqs hand))))

(defn ranks-sorted [hand]
  (sort (ranks hand)))

(defn flush? [hand]
    (apply = (suits hand)))

(defn full-house? [hand]
  (and (= 1 (rank-count is-two? (rank-freqs hand)))
       (= 1 (rank-count is-three? (rank-freqs hand)))))

(defn two-pairs? [hand]
   (= 2 (rank-count is-two? (rank-freqs hand))))

(defn straight? [hand]
  (let [is-straight? (fn [sorted]
                       (let [fst (first sorted)]
                         (= sorted (range fst (+ fst 5)))))]
    (or (is-straight? (ranks-sorted hand))
        (is-straight? (sort (replace {14 1} (ranks hand)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        p (fn [[k v]] (k hand))
        points (map (fn [[k v]] v) (filter p checkers))]
    (apply max points)))
