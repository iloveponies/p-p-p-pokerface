(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rank {\T 10
              \J 11
              \Q 12
              \K 13
              \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rank fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (contains? (set (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (and
      (contains? (set (vals (frequencies ranks))) 3)
      (contains? (set (vals (frequencies ranks))) 2))))

(defn two-pairs? [hand]
  (let [ranks (set (map rank hand))]
    (or
      (and
        (== (count ranks) 3)
        (not (three-of-a-kind? hand)))
      (== (count ranks) 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high-ace-hand (sort ranks)
        low-ace-hand (sort (replace {14 1} high-ace-hand))]
    (or
      (= high-ace-hand (range (apply min ranks) (+ (apply min ranks) 5)))
      (= low-ace-hand (range 1 6)))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        evals (filter (fn [check] ((first check) hand)) checkers)]
    (apply max (map second evals))))
