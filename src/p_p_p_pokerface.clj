(ns p-p-p-pokerface)

(def char-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (char-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn kinds? [n hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (not (empty? (filter (fn [x] (= n x)) freqs)))))

(defn pair? [hand]
  (kinds? 2 hand))

(defn three-of-a-kind? [hand]
  (kinds? 3 hand))

(defn four-of-a-kind? [hand]
  (kinds? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (empty? (rest freqs))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        filtered-freqs (filter (fn [x] (= 2 x)) freqs)]
    (or (not (empty? (rest filtered-freqs))) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        max-rank (apply max ranks)
        sorted-ranks (sort ranks)
        ace-sorted-ranks (sort (replace {14 1} ranks))]
    (if (= (range min-rank (+ max-rank 1)) sorted-ranks)
      true
      (= (range (apply min ace-sorted-ranks) (+ (apply max ace-sorted-ranks) 1)) ace-sorted-ranks))))

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
        matches (filter (fn [x] ((first x) hand)) checkers)]
    (apply max (map second matches))))
