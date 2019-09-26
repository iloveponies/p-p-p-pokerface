(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        valueMap {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else (get valueMap fst))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))]
    (= freq [1 1 1 2])))

;        freq  (vals (frequencies ranks))
;        most  (apply max freq)]
;    (= most 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))]
    (= freq [1 1 3])))

;        freq  (vals (frequencies ranks))
;        most  (apply max freq)]
;    (= most 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq  (vals (frequencies ranks))
        most  (apply max freq)]
    (= most 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (count (set suits)) 1)))


(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq  (sort (vals (frequencies ranks)))]
    (= freq [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq  (sort (vals (frequencies ranks)))]
    (= freq [1 2 2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        rplce (sort (replace {1 14, 14 1} ranks))
        fRanks (first ranks)
        fRplce (first rplce)
        ]
    (or (= ranks (range fRanks (+ fRanks 5)))
        (= rplce (range fRplce (+ fRplce 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
    (cond (pair? hand)            1
          (two-pairs? hand)       2
          (three-of-a-kind? hand) 3
          (straight-flush? hand)  8 ;; this must go before straight and flush
          (straight? hand)        4
          (flush? hand)           5
          (full-house? hand)      6
          (four-of-a-kind? hand)  7
          :else                   0))

