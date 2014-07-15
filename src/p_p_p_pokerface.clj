(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[rank _] card
        court-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (court-cards rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn x-of-a-kind? [hand x]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) x)))

(defn pair? [hand]
  (x-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))
        rank-frequencies (frequencies rank-frequencies)]
  (or (four-of-a-kind? hand)
      (= 2 (get rank-frequencies 2)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        contains-low-ace? (and (contains? (set ranks) 14)
                               (not (contains? (set ranks) 13)))
        ranks (if contains-low-ace?
                (sort (replace {14 1} ranks))
                ranks)
        bottom-rank (first ranks)
        top-rank (last ranks)]
    (= (range bottom-rank (+ 1 top-rank)) ranks)))

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
        all-hands (fn [hand] (map #(when ((first %) hand) (second %)) checkers))
        best-hand (fn [hand] (apply max (remove nil? (all-hands hand))))]
    (best-hand hand)))
