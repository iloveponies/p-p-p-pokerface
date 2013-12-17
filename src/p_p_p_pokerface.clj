(ns p-p-p-pokerface)


(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (boolean (some #(= n %) (vals freqs)))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)
        max-freq (apply max (vals freqs))]
    (= max-freq 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
        twos-filter (fn [x] (= 2 x))
        filtered-values (filter twos-filter values)]
    (= (count filtered-values) 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-ace-low (replace { 14 1 } ranks)
        sorted (sort ranks)
        sorted-ace-low (sort ranks-ace-low)
        min-rank (apply min sorted)
        min-rank-ace-low (apply min sorted-ace-low)
        rank-range (range min-rank (+ min-rank 5))
        rank-range-ace-low (range min-rank-ace-low (+ min-rank-ace-low 5))]
    (or
      (= (seq sorted) (seq rank-range))
      (= (seq sorted-ace-low) (seq rank-range-ace-low)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
