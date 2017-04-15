(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank) (Integer/valueOf(str rank))
      (cond
       (= "T" (str rank)) 10
       (= "J" (str rank)) 11
       (= "Q" (str rank)) 12
       (= "K" (str rank)) 13
       (= "A" (str rank)) 14
       :else nil))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        freqset (set freq)]
  (if (contains? freqset 2)true false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        freqset (set freq)]
  (if (contains? freqset 3)true false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        freqset (set freq)]
  (if (contains? freqset 4)true false)))

(defn flush? [hand]
    (let [suits (map suit hand)
        freq (vals (frequencies suits))
        freqset (set freq)]
  (if (contains? freqset 5)true false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        freqset (set freq)]
  (if (contains? freqset 2) (contains? freqset 3) false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))
        freqset (set freq)]
  (if (contains? freqset 2) (= 3 (count freq)) false)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks-ace-14 (sort ranks)
        min-rank-ace-14 (apply min sorted-ranks-ace-14)
        max-rank-ace-14 (apply max sorted-ranks-ace-14)
        ranks-ace-1 (replace {14 1} sorted-ranks-ace-14)
        sorted-ranks-ace-1 (sort ranks-ace-1)
        min-rank-ace-1 (apply min sorted-ranks-ace-1)
        max-rank-ace-1 (apply max sorted-ranks-ace-1)
        freq (vals (frequencies ranks))
        ]
    (if (== (count freq) 5)
      (if (== (- min-rank-ace-14 max-rank-ace-14) -4) true (== (- min-rank-ace-1 max-rank-ace-1) -4))
      false)))

(defn straight-flush? [hand]
  (if (straight? hand) (flush? hand) false))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
    ((get checkers value) hand)
  ))

(defn value [hand]
  (cond
   (hand-has-value? hand 8) 8
   (hand-has-value? hand 7) 7
   (hand-has-value? hand 6) 6
   (hand-has-value? hand 5) 5
   (hand-has-value? hand 4) 4
   (hand-has-value? hand 3) 3
   (hand-has-value? hand 2) 2
   (hand-has-value? hand 1) 1
   :else 0)
  )



