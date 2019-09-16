(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank_rep {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r s] card]
    (if (Character/isDigit r)
         (Integer/valueOf (str r))
         (rank_rep r))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn max-number-of-ranked-cards [hand m]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        v (vals freqs)
        mx (apply max v)]
    (= m mx)))

(defn pair? [hand]
  (max-number-of-ranked-cards hand 2))

(defn three-of-a-kind? [hand]
  (max-number-of-ranked-cards hand 3))

(defn four-of-a-kind? [hand]
  (max-number-of-ranked-cards hand 4))

(defn flush? [hand]
  (let [s (map suit hand)
        freqs (frequencies s)
        k (keys freqs)]
    (= 1 (count k))))

(defn full-house? [hand]
  (let [r (map rank hand)
        freqs (frequencies r)
        v (sort (vals freqs))]
    (= v [2 3])))


(defn two-pairs? [hand]
  (let [r (map rank hand)
        freqs (frequencies r)
        v (sort (vals freqs))]
    (= v [1 2 2])))

(defn _straight [r]
  (let [_max (apply max r)
        _min (apply min r)
        _upper (+ 1 _max)
        _rangedh (take 5 (range _min _upper))
        _sortedh (sort r)]
    (= _rangedh _sortedh)))

(defn straight? [hand]
  (let [r (map rank hand)]
    (or (_straight (replace {14 1} r))
        (_straight r))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map #(second %) (filter #((first %) hand) checkers)))))
