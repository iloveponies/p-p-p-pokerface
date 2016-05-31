(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (cond (Character/isDigit fst) (Integer/valueOf (str fst))
           :else (replacements fst))))

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
  (= (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (and
    (contains? (set (vals (frequencies (map rank hand)))) 2)
    (contains? (set (vals (frequencies (map rank hand)))) 3)))

(defn two-pairs? [hand]
  (and
    (= (count (vals (frequencies (map rank hand)))) 3)
    (not (three-of-a-kind? hand))))

(defn straight? [hand]
  (let [handseq (sort (map rank hand))]
    (if (= handseq [2,3,4,5,14]) true
      (let [start (first handseq)]
        (= handseq (range start (+ start 5)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
  (apply max (map second (filter (fn[x] ((first x) hand)) checkers)))))
