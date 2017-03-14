(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [c _] card]
    (if (Character/isDigit c)
        (Integer/valueOf (str c))
        (get replacements c))))

(defn suit [card]
  (let [[c s] card]
    (str s)))

(defn n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        has-n-of-a-kind? (fn [[rank freq]] (== freq n))]
    (boolean (some has-n-of-a-kind? rank-freq))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [two-pairs (filter (fn [[rank freq]] (== freq 2))
                          (frequencies (map rank hand)))
        pair-count (count two-pairs)]
  (or (four-of-a-kind? hand)
      (== pair-count 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-straight? #(let [sorted (sort %)
                               range-start (first sorted)
                               range-end (+ range-start 5)
                               expected (range range-start range-end)]
                            (= sorted expected))]
    (or (ranks-straight? ranks)
        (ranks-straight? (replace {14 1} ranks)))))

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
                   [straight-flush? 8]}
        checker-to-value (fn [checker] (let [checker-func (first checker)
                                             checker-value (second checker)]
                                          (if (checker-func hand)
                                              checker-value
                                              0)))
        checker-results (map checker-to-value checkers)]
    (apply max checker-results)))
