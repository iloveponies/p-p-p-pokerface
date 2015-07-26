(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn max-of-a-kind [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (apply max freqs)))

(defn pair? [hand]
  (<= 2 (max-of-a-kind hand)))
;13

(defn three-of-a-kind? [hand]
  (<= 3 (max-of-a-kind hand)))
;15

(defn four-of-a-kind? [hand]
  (<= 4 (max-of-a-kind hand)))
;17

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 1 (count freqs))))
;19

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= [2 3] freqs)))
;22

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= [1 2 2] freqs)))
;25

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min-rank (apply min ranks)
        max-rank (apply max ranks)]
    (or
      (= (range min-rank (+ 1 max-rank)) ranks)
      (= [2 3 4 5 14] ranks))))
;30

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))
;34

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matches (map
                   (fn [check]
                     (if ((first check) hand)
                         (second check)
                          0))
                    checkers)]
    (apply max matches)))
