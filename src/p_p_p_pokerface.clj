(ns p-p-p-pokerface)


(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [rank-char _] card]
    (cond
     (Character/isDigit rank-char) (Integer/parseInt (str rank-char))
     :else (replacements rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)]
    (contains? (set (vals rank-freq)) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= 1 (count suits))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freq (frequencies ranks)
        freq-freq (frequencies (vals rank-freq))]
    (or (= 2 (freq-freq 2))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        adjusted-ranks (cond
                        (contains? (set ranks) 2) (replace {14 1} ranks)
                        :else ranks)
        sorted-ranks (sort adjusted-ranks)
        min-rank (apply min sorted-ranks)]
    (= sorted-ranks
       (range min-rank (+ min-rank 5)))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [[high-card? 0]
                  [pair? 1]
                  [two-pairs? 2]
                  [three-of-a-kind? 3]
                  [straight? 4]
                  [flush? 5]
                  [full-house? 6]
                  [four-of-a-kind? 7]
                  [straight-flush? 8]]
        matches (filter (fn [checker]
                          ((first checker) hand))
                        checkers)
        values (map second matches)]
    (apply max values)))

