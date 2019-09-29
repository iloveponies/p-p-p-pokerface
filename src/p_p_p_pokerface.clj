(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn seq-contains? [seq val]
  (not= (some #(= val %) seq) nil))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [freqs (frequencies (map rank hand))]
    (seq-contains? (vals freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (seq-contains? (vals freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (seq-contains? (vals freqs) 4)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand))
        sorted (sort (vals freqs))]
    (= sorted '(1 2 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted (sort ranks)
        aced (if (< (apply min sorted) 10)
               (replace {14 1} sorted)
               sorted)]
    (and (== 5 (count (set aced)))
         (== 4 (- (apply max aced) (apply min aced))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches? (fn [checker] ((first checker) hand))
        matches (filter matches? checkers)
        values (map second matches)
        maxvalue (apply max values)]
    maxvalue))

