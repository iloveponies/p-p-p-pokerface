(ns p-p-p-pokerface)

(defn replacement [a-char]
  (get {\T 10, \J 11, \Q 12, \K 13, \A 14} a-char))

(defn rank [card]
  (let [[fst _] card]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else                   (replacement fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (cond
    (four-of-a-kind? hand)                                  true
    (= [1 2 2] (sort (vals (frequencies (map rank hand))))) true
    :else                                                   false))

(defn five-of-seq-rank? [fst sorted-hand]
  (= sorted-hand (range fst (+ fst 5))))

(defn straight? [hand]
   (let [sorted-hand (sort (mapv rank hand))
         [a b c d e] sorted-hand]
     (cond
       (not (== e 14)) (five-of-seq-rank? a sorted-hand)
       (== a 2)        (five-of-seq-rank? 1 (sort (replace {e 1} sorted-hand)))
       (== d 13)       (five-of-seq-rank? a sorted-hand)
       :else           false
       )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

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
    :else                   0
    ))
