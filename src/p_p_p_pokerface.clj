(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank, _] card
        is-digit? (Character/isDigit rank)
        char-map {\T 10, \J 11, \Q 12, \K 13, \A 14}
        rank-char (if is-digit?
                     rank
                     (get char-map rank))]
    (Integer/valueOf (str rank-char))))

(defn suit [card]
  (let [[_, suit] card]
    (str suit)))

(defn freq-of-n-of-a-kind [hand, n]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))
        count-freqs (frequencies rank-freqs)
        number (get count-freqs n)]
    (if (nil? number)
      0
      number)))

(defn n-of-suit? [hand, n]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (boolean (some #{n} freqs))))

(defn pair? [hand]
  (>= (freq-of-n-of-a-kind hand 2) 1))

(defn three-of-a-kind? [hand]
  (>= (freq-of-n-of-a-kind hand 3) 1))

(defn four-of-a-kind? [hand]
  (>= (freq-of-n-of-a-kind hand 4) 1))

(defn flush? [hand]
  (n-of-suit? hand 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (>= (freq-of-n-of-a-kind hand 2) 2))

(defn straight? [hand]
  (let [_straight? (fn [ranks]
                      (let [ranks (sort ranks)
                            lowest (first ranks)
                            straight (range lowest (+ lowest 5))]
                        (= ranks straight)))
        ranks (map rank hand)
        low-ranks (replace {14 1} ranks)]
    (or (_straight? ranks)
        (_straight? low-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

; (defn high-card? [hand]
;   true)

; (defn value [hand]
;   (let [checkers #{[high-card? 0]  [pair? 1]
;                    [two-pairs? 2]  [three-of-a-kind? 3]
;                    [straight? 4]   [flush? 5]
;                    [full-house? 6] [four-of-a-kind? 7]
;                    [straight-flush? 8]}
;         valid-checkers (filter (fn [[checker, _]] (checker hand)) checkers)
;         valid-values (map (fn [[_, value]] value) valid-checkers)]
;     (apply max valid-values)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
    ))
