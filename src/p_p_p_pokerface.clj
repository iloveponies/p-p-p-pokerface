(ns p-p-p-pokerface)

(defn rank [[card-rank _]]
  (if (Character/isDigit card-rank)
    (Integer/valueOf (str card-rank))
    (let [face-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (get face-ranks card-rank))))

(defn suit [[_ card-suit]]
  (str card-suit))

(defn- rank-frequencies [hand]
  (->> (map rank hand)
       frequencies
       vals))

(defn- suit-frequencies [hand]
  (->> (map suit hand)
        frequencies
        vals))

(defn- n-of-a-kind? [hand n]
  (contains? (set (rank-frequencies hand)) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= 1 (count (suit-frequencies hand))))

(defn full-house? [hand]
  (= #{2 3} (set (rank-frequencies hand))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (rank-frequencies hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (if (and (contains? (set ranks) 14) (not (contains? (set ranks) 13)))
                       (sort (replace {14 1} ranks))
                       (sort ranks))
        straight (range (first sorted-ranks) (inc (last sorted-ranks)))]
    (= sorted-ranks straight)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matches (filter #((first %) hand) checkers)
        values (map #(second %) matches)]
    (apply max values)))