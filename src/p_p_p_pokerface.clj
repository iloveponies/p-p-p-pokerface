(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
      )))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 1)
    true
    false
  ))

(defn three-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false
  ))

(defn four-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 3)
    true
    false
  ))

(defn flush? [hand]
  (if (= (apply max (vals (frequencies (map suit hand)))) 5)
    true
    false
  ))

(defn full-house? [hand]
  (if (and (= (apply max (vals (frequencies (map rank hand)))) 3)(= (apply min (vals (frequencies (map rank hand)))) 2))
    true
    false
  ))

(defn two-pairs? [hand]
  (if (or (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 2) (= (count(filter #(= 4 %) (vals (frequencies (map rank hand))))) 1))
    true
    false))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alt-ranks (replace {14 1} ranks)
        minranks (apply min ranks)
        minal (apply min alt-ranks)]
    (or (= (range minranks (+ minranks 5))(sort ranks))
        (= (range minal (+ minal 5))(sort alt-ranks))
        )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)
       ))
(defn high-card? [hand]
  true)

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        (high-card? hand) 0
    ))
