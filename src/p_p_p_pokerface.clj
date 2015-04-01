(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (vals (frequencies ranks)))]
    (= [2 3] sorted-ranks)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (vals (frequencies ranks)))]
    (or (= [1 2 2] sorted-ranks)
        (= [1 4] sorted-ranks))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        fst (first sorted-ranks)
        ace1-ranks (sort (map #(if (= % 14) 1 %) ranks))
        ace1-fst (first ace1-ranks)]
    (or (= (range fst (+ fst 5)) sorted-ranks)
        (= (range ace1-fst (+ ace1-fst 5)) ace1-ranks))))

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
                   [straight-flush? 8]}
        check-fn (fn [[f _]]
                   (f hand))
        check-results (filter check-fn checkers)
        values (map second check-results)]
    (apply max values)))
