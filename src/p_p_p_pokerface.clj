(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[r _] card
        codes {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
        (Integer/valueOf (str r))
        (get codes r))))

(defn suit [card]
  (let[[_ s] card]
    (str s)))

(defn rank-groups [hand]
  (vals (frequencies (map rank hand))))

(defn kind [hand n]
  (>= (apply max (rank-groups hand)) n))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (kind hand 2))

(defn three-of-a-kind? [hand]
  (kind hand 3))

(defn four-of-a-kind? [hand]
  (kind hand 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= #{2, 3} (set (rank-groups hand))))

(defn two-pairs? [hand]
  (<= (count (filter #{1} (rank-groups hand))) 1))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or (= ranks [2 3 4 5 14])
        (and (= (+ 4 (first ranks)) (last ranks)) (apply < ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
