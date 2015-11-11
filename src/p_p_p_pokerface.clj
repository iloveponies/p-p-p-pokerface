(ns p-p-p-pokerface)

(defn rank [card]
  (let [[nro _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        ->int (fn [type] (Integer/valueOf (type nro)))]
    (if (Character/isDigit nro)
      (->int str)
      (->int replacements))))

(defn suit [card]
  (let [[_ maa] card]
    (str maa)))

(defn ranks [hand]
  (sort (map rank hand)))

(defn rank-appearances [hand]
  (sort (vals (frequencies (ranks hand)))))

(defn same-ranks? [hand number]
  (== number (apply max (rank-appearances hand))))

(defn pair? [hand]
  (same-ranks? hand 2))

(defn three-of-a-kind? [hand]
  (same-ranks? hand 3))

(defn four-of-a-kind? [hand]
  (same-ranks? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (= [2 3] (rank-appearances hand)))

(defn rank-appears? [hand seq]
  (= seq (rank-appearances hand)))

(defn two-pairs? [hand]
  (or (rank-appears? hand [1 2 2]) (rank-appears? hand [1 4])))

(defn straight? [hand]
  (let [[_ _ _ a korkein] (ranks hand)
        comp [(- korkein 4) (- korkein 3) (- korkein 2) (- korkein 1) korkein]]
    (if (and (not (= a 13)) (= korkein 14))
      (= [2 3 4 5 14] (ranks hand))
      (= comp (ranks hand))
      )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

;(defn value [hand]
;  (let [checkers #{[high-card? 0]  [pair? 1]
;                   [two-pairs? 2]  [three-of-a-kind? 3]
;                   [straight? 4]   [flush? 5]
;                   [full-house? 6] [four-of-a-kind? 7]
;                   [straight-flush? 8]}
;        hand-types (filter (fn [x] ((first x) hand)) checkers)
;        values (map second (hand-types))]
;    (apply max values)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))
