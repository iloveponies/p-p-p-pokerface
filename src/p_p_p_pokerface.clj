(ns p-p-p-pokerface)


(defn rank [card]
  (let [[rank _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (if (some #(= 2 %) rank-freqs)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (if (some #(= 3 %) rank-freqs)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (if (some #(= 4 %) rank-freqs)
      true
      false)))

(defn flush? [hand]
  (let [suit-freqs (vals (frequencies (map suit hand)))]
    (if (some #(= 5 %) suit-freqs)
      true
      false)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (= [1 2 2] (sort rank-freqs))))

(defn straight? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))
        greatest-rank (apply max (map rank hand))
        smallest-rank (apply min (map rank hand))]
    (and
      (= (apply max rank-freqs) 1)
      (or (= (- greatest-rank smallest-rank) 4)
          (= (- greatest-rank smallest-rank) 12))))) ;low-ace-straight with A-rank as 14


(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)



(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
       (filter (fn [x] checkers))))
