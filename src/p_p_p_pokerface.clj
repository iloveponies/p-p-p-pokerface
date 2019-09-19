(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
      charmap {\T 10 \J 11 \Q 12 \K 13 \A 14}
      getcharvalue (fn [c] (get charmap c))]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (getcharvalue r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn freq [minormax rankorsuit hand]
  (apply minormax (vals (frequencies (map rankorsuit hand)))))

(defn rankfreqs [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (> (freq max rank hand) 1))

(defn three-of-a-kind? [hand]
  (> (freq max rank hand) 2))

(defn four-of-a-kind? [hand]
  (> (freq max rank hand) 3))

(defn flush? [hand]
  (== (freq max suit hand) 5))

(defn full-house? [hand]
  (and
   (three-of-a-kind? hand)
   (== 2 (freq min rank hand))))

(defn two-pairs? [hand]
  (let [pairscount
        (fn [hand] (count (filter (fn [x] (= x 2)) (rankfreqs hand))))]
    (> (pairscount hand) 1)))

(defn straight? [hand]
  (let [sortedranks (sort (map rank hand))
        firstrank (first sortedranks)
        lowacestraight [2 3 4 5 14]]
    (or
      (= sortedranks (range firstrank (+ firstrank 5)))
      (= sortedranks lowacestraight))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [check] ((first check) hand)) checkers)))))
