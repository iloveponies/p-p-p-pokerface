(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[rank]]
  (cond
    (Character/isDigit rank) (Integer/valueOf (str rank))
    :else (ranks rank)))

(defn suit [[_ suit]]
  (str suit))

(defn n-of-a-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (== 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= #{2 3} (set (vals (frequencies (map rank hand))))))

;; Kinda hacky, definitely not ideal

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (== 2
        (count (filter (fn [x] (>= x 2))
                       (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [ranks (mapv rank hand)
        alter-ranks (replace {14 1} ranks)]
    (and
      (== 5 (count (set ranks)))
      (or
        (== 4(- (apply max ranks) (apply min ranks)))
        (== 4(- (apply max alter-ranks) (apply min alter-ranks)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [pair]
                      (if ((first pair) hand) (second pair) 0)) checkers))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
