(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [cnts (vals (frequencies (map rank hand)))]
    (>= (apply max cnts) 2)))

(defn three-of-a-kind? [hand]
  (let [cnts (vals (frequencies (map rank hand)))]
    (>= (apply max cnts) 3)))

(defn four-of-a-kind? [hand]
  (let [cnts (vals (frequencies (map rank hand)))]
    (>= (apply max cnts) 4)))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= 1 (count suits))))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (or (= ranks [2 3])
        (= ranks [3 2]))))

(defn two-pairs? [hand]
  (let [ranks (sort (vals (frequencies (map rank hand))))]
    (or (= ranks [1 2 2])
        (= ranks [1 4]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-high-ranks (sort ranks)
        ace-low-ranks (sort (replace {14 1} ranks))
        ace-high-low-card (first ace-high-ranks)
        ace-low-low-card (first ace-low-ranks)
        ace-high-adjusted-ranks (map (fn [x] (- x ace-high-low-card)) ace-high-ranks)
        ace-low-adjusted-ranks (map (fn [x] (- x ace-low-low-card)) ace-low-ranks)]
    (or (= ace-high-adjusted-ranks [0 1 2 3 4])
        (= ace-low-adjusted-ranks [0 1 2 3 4]))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        true-checkers (filter (fn [x] ((first x) hand)) checkers)
        values (map second true-checkers)]
    (apply max values)))
