(ns p-p-p-pokerface)

(defn- consecutive? [items]
  (let [items (sort items)]
    (= items (range (first items) (inc (last items))))))

(defn- max-rank-frequency [ranks]
  (apply max (vals (frequencies ranks))))

(defn rank [card]
  (let [[rank _] card
        char-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (char-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (>= (max-rank-frequency ranks) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (max-rank-frequency ranks) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (max-rank-frequency ranks) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (= [2 3] (sort rank-freq-vals))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-frequencies (frequencies ranks)
        rank-pairs (filter #(>= (val %) 2) rank-frequencies)]
    (>= (count rank-pairs) 2)))

(defn straight? [hand]
  (let [high-ace-ranks (map rank hand)
        low-ace-ranks (replace {14 1} high-ace-ranks)]
    (or
      (consecutive? high-ace-ranks)
      (consecutive? low-ace-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        passing-checkers (filter #((first %) hand) checkers)
        passing-checker-values (map second passing-checkers)]
    (apply max passing-checker-values)))
