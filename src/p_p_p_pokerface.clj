(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [ranks (set (vals (frequencies (map rank hand))))]
    (and (contains? ranks 2)
         (contains? ranks 3))))

(defn two-pairs? [hand]
  (let [ranks (sort (vals (frequencies (map rank hand))))]
    (or (= ranks [1 2 2])
        (= ranks [1 4]))))

(defn straight? [hand]
   (let [ranks (sort (map rank hand))]
     (or (= ranks [2 3 4 5 14])
         (= ranks (range (apply min ranks) (+ (apply min ranks) 5))))))

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
    (let [check (fn [[score-hand points]] (score-hand hand))]
     (apply max (map second (filter check checkers))))))
