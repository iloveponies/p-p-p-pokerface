(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [cnt (frequencies (vals (frequencies (map rank hand))))]
    (if (and (= (get cnt 2) 1) (= (get cnt 1) 3))
    true
    false)))

(defn three-of-a-kind? [hand]
  (let [cnt (frequencies (vals (frequencies (map rank hand))))]
    (if (and (= (get cnt 3) 1) (= (get cnt 1) 2))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [cnt (frequencies (vals (frequencies (map rank hand))))]
    (if (and (= (get cnt 4) 1) (= (get cnt 1) 1))
      true
      false)))

(defn flush? [hand]
  (let [cnt (frequencies (vals (frequencies (map suit hand))))]
    (if (= (get cnt 5) 1)
      true
      false)))

(defn full-house? [hand]
  (let [cnt (frequencies (vals (frequencies (map rank hand))))]
    (if (and (= (get cnt 3) 1) (= (get cnt 2) 1))
      true
      false)))


(defn two-pairs? [hand]
  (let [cnt (frequencies (vals (frequencies (map rank hand))))]
    (if (and (= (get cnt 2) 2) (= (get cnt 1) 1))
      true
      false)))

(defn straight? [hand]
  (let [cards (sort (map rank hand))
        cards (sort (if (= (first cards) 2)
                      (replace {14 1} cards)
                      cards))
        min (apply min cards)
        max (apply max cards)]
    (if (= cards (range min (+ max 1)))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
