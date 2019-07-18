(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank-char))))

(defn suit [card]
  (let [[_ suit-character] card]
    (str suit-character)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        freq-ranks (vals (frequencies ranks))]
    (contains? (set freq-ranks) n)))

; All these hand category functions test for "at least"
; I.E. a four of a kind hand wil have pair?, three-of-a-kind? and
; four-of-a-kind? all return true.
(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq-ranks (vals (frequencies ranks))
        freq-freq (frequencies freq-ranks)]
    (= 2 (get freq-freq 2))))

(defn sequential-ranks? [ranks]
  (let [lowest-rank (apply min ranks)
        needed-seq (range lowest-rank (+ 5 lowest-rank))
        sorted-hand (sort ranks)]
    (= needed-seq sorted-hand)))

(defn straight? [hand]
  "NB. Will also return true for a straight flush."
  (let [ranks (map rank hand)
        ranks-ace-as-1 (replace {14 1} ranks)]
    (or
      (sequential-ranks? ranks)
      (sequential-ranks? ranks-ace-as-1))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [_]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        passed-checks (filter (fn [checker] ((first checker) hand)) checkers)
        passed-ranks (map second passed-checks)]
    (apply max passed-ranks)))
