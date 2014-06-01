(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (replacements r))))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (let [rs (map rank hand)]
    (> (apply max (vals (frequencies rs))) 1)))

(defn three-of-a-kind? [hand]
  (let [rs (map rank hand)]
    (> (apply max (vals (frequencies rs))) 2)))

(defn four-of-a-kind? [hand]
  (let [rs (map rank hand)]
    (> (apply max (vals (frequencies rs))) 3)))


(defn flush? [hand]
  (let [ss (set (map suit hand))]
    (= (count ss) 1)))

(defn full-house? [hand]
  (let [rs (-> (map rank hand)
               (frequencies)
               (vals)
               (set))]
    (= rs #{2 3})))

(defn two-pairs? [hand]
  (let [rs (-> (map rank hand)
               (frequencies)
               (vals)
               (sort))]
    (or (= rs '(1 2 2))
        (= rs '(1 4)))))

(defn straight? [hand]
  (let [rs (sort (map rank hand))
        rs' (range (first rs) (+ (first rs) 5))]
    (if (= rs rs')
      true
      (let [rs1 (sort (replace {14 1} rs)) 
            rs1' (range (first rs1) (+ (first rs1) 5)) ]
        (= rs1 rs1')))))

(defn straight-flush? [hand]
  (and (flush? hand) (sequential-rank? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hands (filter (fn [[c v]] (c hand)) checkers)
        values (map second hands)]
    (apply max values)))
