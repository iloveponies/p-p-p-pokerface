(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  ;; (= (count (set (map suit hand))) 1))
  (if (apply = (map suit hand))
    true
    false))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2]))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lower (apply min ranks)
        upper (apply max ranks)]
    (if (apply < ranks)
      (if (and (= lower 2) (= upper 14))
        ;; (= (sort (replace {upper 1} ranks)) (range 1 6))
        (= ranks [2 3 4 5 14])
        (= ranks (range lower (+ upper 1))))
      false)))

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
                   [straight-flush? 8]}]
    (apply max (map second
                    (filter (fn [func] ((first func) hand))
                            checkers)))))
