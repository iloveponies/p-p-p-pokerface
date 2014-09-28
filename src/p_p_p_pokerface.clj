(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get replacements fst))))

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
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [ranks (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] ranks)
        (= [1 4] ranks))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks))
      consec (fn [a b c d e] (== (+ a 4) (+ b 3) (+ c 2) (+ d 1) e))]
    (or (apply consec ranks)
      (apply consec ranks2))))

(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
