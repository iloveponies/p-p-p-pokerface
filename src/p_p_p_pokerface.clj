(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        conversions {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (conversions fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (filter (fn [x] (if (<= 2 x) true false)) (vals (frequencies ranks)))]
    (if (empty? freqs)
      false
      true)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (filter (fn [x] (if (<= 3 x) true false)) (vals (frequencies ranks)))]
    (if (empty? freqs)
      false
      true)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (filter (fn [x] (if (<= 4 x) true false)) (vals (frequencies ranks)))]
    (if (empty? freqs)
      false
      true)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (if (= 1 (count freqs))
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (set (filter (fn [x] (if (or (= 2 x) (= 3 x)) true false)) (vals (frequencies ranks))))]
    (if (= 2 (count freqs))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (filter (fn [x] (if (<= 2 x) true false)) (vals (frequencies ranks)))]
    (if (= 2 (count freqs))
      true
      false)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min (apply min ranks)
        max (apply max ranks)]
    (if (or (= (range min (+ 1 max)) ranks)
            (and (= max 14)
                 (or (= min 2) (= min 10)) (= (range min (+ 4 min)) (drop-last ranks))))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        filtered-checkers (filter (fn [checker] (if ((first checker) hand) true false)) checkers)
        values (map second filtered-checkers)]
    (apply max values)))
