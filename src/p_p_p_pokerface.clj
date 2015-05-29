(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn max-frequency-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (> (max-frequency-rank hand) 1))

(defn three-of-a-kind? [hand]
  (> (max-frequency-rank hand) 2))

(defn four-of-a-kind? [hand]
  (> (max-frequency-rank hand) 3))

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (= (vals (frequencies (map rank hand))) (seq [3 2])))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2])) ))

(defn straight-alternative? [hand]
  (let [mapped-hand (sort (map rank hand))
        lowest-rank (apply min mapped-hand)]
    (or (= mapped-hand (seq [2 3 4 5 14]))
      (= mapped-hand (range lowest-rank (+ lowest-rank 5))))))

(defn straight? [hand]
  (let [mapped-hand (sort (map rank hand))
        mapped-hand-replaced-a (sort (replace {14 1} mapped-hand))
        lowest-rank (fn [rank-seq] (apply min rank-seq))
        straight-order (fn [rank-seq] (range (lowest-rank rank-seq) (+ (lowest-rank rank-seq) 5)))]
    (or (= mapped-hand (straight-order mapped-hand))
      (= mapped-hand-replaced-a (straight-order mapped-hand-replaced-a)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[(high-card? hand) 0]  [(pair? hand) 1]
                 [(two-pairs? hand) 2]  [(three-of-a-kind? hand) 3]
                 [(straight? hand) 4]   [(flush? hand) 5]
                 [(full-house? hand) 6] [(four-of-a-kind? hand) 7]
                 [(straight-flush? hand) 8]}]
    (apply max (map second (filter (fn [pair] (first pair)) checkers)))))
