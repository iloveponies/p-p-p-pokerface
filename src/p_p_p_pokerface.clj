(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn freqs [hand f]
  (vals (frequencies (map f hand))))

(defn max-frequency [hand f]
  (apply max (freqs hand f)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (= (max-frequency hand rank) 2))

(defn three-of-a-kind? [hand]
  (= (max-frequency hand rank) 3))

(defn four-of-a-kind? [hand]
  (= (max-frequency hand rank) 4))

(defn flush? [hand]
  (= (max-frequency hand suit) 5))

(defn full-house? [hand]
  (= (sort (freqs hand rank)) [2 3]))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= (sort (freqs hand rank)) [1 2 2])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace-ranks (replace {14 1} ranks)
        continous? (fn [r] (= (sort r) (range (apply min r) (+ (apply min r) 5))))]
    (or (continous? ranks) (continous? low-ace-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
  ((checkers value) hand)))

(defn hand-has-type? [hand checker-value]
  (let [[checker _] checker-value]
    (checker hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filter (fn [checker] (hand-has-type? hand checker)) checkers)]
    ;(apply max (filter (fn [value] (hand-has-value? hand value)) (range 0 9)))))
    (apply max (map second matching-checkers))))
