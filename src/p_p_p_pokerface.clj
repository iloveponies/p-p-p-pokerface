(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r] card replacements {\T 10
                               \J 11
                               \Q 12
                               \K 13
                               \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (if (= 2 (apply max freq))
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (if (and (= 3 (apply max freq)) (= 1 (apply max (rest freq))))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (if (and (= 4 (apply max freq)) (= 1 (apply max (rest freq))))
      true
      false)))

(defn flush? [hand]
  (let [freq (vals (frequencies (map suit hand)))]
    (if (= 5 (apply max freq))
      true
      false)))

(defn full-house? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (if (= (sort freq) [2 3])
      true
      false)))

(defn two-pairs? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
    (if (or (four-of-a-kind? hand) (= freq [1 2 2]))
      true
      false)))

(defn straight? [hand]
  (let [rankings (sort (map rank hand))
        start (apply min rankings)
        stop (apply max rankings)]
    (if (or (= rankings [2 3 4 5 14]) (= rankings (range start (+ stop 1))))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [table #{[pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                [straight? 4] [flush? 5] [full-house? 6]
               [four-of-a-kind? 7] [straight-flush? 8]
               [high-card? 0]}
        helper (fn [x] ((first x) hand))]
    (apply max ( map second (filter helper table)))))
