(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        face-values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if(Character/isDigit rank)
      (Integer/valueOf (str rank))
      (face-values rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [f (fn [number] (if (== number 2) number))]
    (or (== (count (filter f (vals (frequencies 
      (map rank hand))))) 2) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        lowest-card (first sorted-ranks)
        ideal-straight (range lowest-card (+ lowest-card 5))     
        sorted-ranks-ace-fix (sort (replace {14, 1} sorted-ranks))
        ideal-straight-ace-fix [1 2 3 4 5]]
    (or 
      (= sorted-ranks ideal-straight) 
      (= sorted-ranks-ace-fix ideal-straight-ace-fix))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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