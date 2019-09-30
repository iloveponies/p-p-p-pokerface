(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (replacements first))))

(defn suit [card]
  (let [[_ second] card ]
    (str second)))

(defn rank-frequency [hand]
  (let [ranks (map rank hand)]
    (frequencies ranks)))

(defn suit-frequency [hand]
  (let [suits (map suit hand)]
    (frequencies suits)))

(defn max-frequency [freq-fn hand]
  max-frequency (apply max (vals (freq-fn hand))))

(defn pair? [hand]
    (= (max-frequency rank-frequency hand) 2))

(defn three-of-a-kind? [hand]
  (= (max-frequency rank-frequency hand) 3))

(defn four-of-a-kind? [hand]
  (= (max-frequency rank-frequency hand) 4))

(defn flush? [hand]
  (= (max-frequency suit-frequency hand) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [2 3])))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        max-rank (apply max ranks)
        ranks (if (and (= min-rank 2) (= max-rank 14))
                (replace {14 1} (apply vector ranks))
                ranks)
        min-rank (apply min ranks)
        max-rank (apply max ranks)
        range-ranks (range min-rank (+ max-rank 1))]
    (if (= range-ranks (sort ranks)) true false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check (fn [checker]
                (if ((get checker 0) hand)
                 (get checker 1)
                 false))]
    (apply max (filter max (map check checkers)))))
