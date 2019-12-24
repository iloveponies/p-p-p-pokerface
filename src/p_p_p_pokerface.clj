(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn contains-amount? [hand amount suit-or-rank]
  (>= (apply max (vals (frequencies (map suit-or-rank hand)))) amount))

(defn pair? [hand]
  (contains-amount? hand 2 rank))

(defn three-of-a-kind? [hand]
  (contains-amount? hand 3 rank))

(defn four-of-a-kind? [hand]
  (contains-amount? hand 4 rank))

(defn flush? [hand]
  (contains-amount? hand 5 suit))

(defn sorted-rank [hand]
  (map (fn [card] (rank card)) hand))

(defn sorted-rank-count [hand]
  (sort (vals (frequencies (sorted-rank hand)))))

(defn full-house? [hand]
  (or
    (= [2 3] (sorted-rank-count hand))
    (= [5] (sorted-rank-count hand))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sorted-rank-count hand))
    (= [1 4]   (sorted-rank-count hand))))

; TODO use let
(defn straight? [hand]
  (defn low-sorted [hand]
    (sort (replace {14, 1}(sorted-rank hand))))
  (defn high-sorted [hand]
    (sort (sorted-rank hand)))
  (defn hand-range [sorted-hand]
    (range (apply min sorted-hand) (+ (apply min sorted-hand) 5)))
  (or
    (= (hand-range (low-sorted hand)) (low-sorted hand))
    (= (hand-range (high-sorted hand)) (high-sorted hand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map  second (filter (fn [x] ((first x) hand)) checkers)))))
