(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _ ] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (get replacements fst)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freqs   (frequencies (map rank hand))
        maximum (apply max (vals freqs))]
    (== maximum 2)))

(defn three-of-a-kind? [hand]
  (let [freqs   (frequencies (map rank hand))
        maximum (apply max (vals freqs))]
   (and (== maximum 3) (not (pair? hand)))))

(defn four-of-a-kind? [hand]
  (let [freqs   (frequencies (map rank hand))
        maximum (apply max (vals freqs))]
    (== maximum 4)))

(defn flush? [hand]
  (let [suits      (map suit hand)
        ranks      (sort (map rank hand))
        min-rank   (apply min ranks)
        rank-freqs (vals (frequencies ranks))
        suit-freqs (vals (frequencies suits))]
    (and (= (apply max rank-freqs) 1)
         (= (apply max suit-freqs) 5)
         (not= ranks (range min-rank (+ min-rank 5))))))

(defn full-house? [hand]
  (let [ranks (sort (map rank hand))
        freqs (vals (frequencies ranks))]
    (= freqs (seq [3 2]))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= freqs (seq [2 2 1]))))

(defn straight? [hand]
  (let [ranks   (sort (map rank hand))
        minimum (apply min ranks)
        freqs   (vals (frequencies ranks))]
    (or (= ranks (range minimum (+ minimum 5)))
        (= ranks (seq [2 3 4 5 14]))
        (= ranks (seq [10 11 12 13 14])))))

(defn straight-flush? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))
        suit-freqs (vals (frequencies (map suit hand)))]
    (and (straight? hand)
         (= (apply max rank-freqs) 1)
         (= (apply max suit-freqs) 5))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        apply-checker  (fn [checker] ((first checker) hand))
        matching-hands (filter apply-checker checkers)
        hand-values    (map second matching-hands)]
    (apply max hand-values)))
