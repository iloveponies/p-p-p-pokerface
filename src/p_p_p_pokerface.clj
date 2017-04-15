(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (if (Character/isDigit rank)
        (Integer/valueOf(str rank))
        (replacements rank))))

(defn suit [card]
  (let [[_ color] card]
    (str color)))

(defn hand-contains-x-of-a-kind? [hand x]
  (<= x (apply max (vals (frequencies (map rank hand))))))

(defn all-cards-have-atleast-x-of-the-same-kind? [hand x]
  (<= x (apply min (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (hand-contains-x-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (hand-contains-x-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (hand-contains-x-of-a-kind? hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand))
       (all-cards-have-atleast-x-of-the-same-kind? hand 2))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
       (four-of-a-kind? hand)
       (full-house? hand)))

(defn straight? [hand]
  (let [sorted-hand-map (sort (map rank hand))
        sorted-hand-map-14-as-1 (sort (replace {14 1} sorted-hand-map))
        max-card (apply max sorted-hand-map)
        min-card (apply min sorted-hand-map)]

    (if (and (= max-card 14) (= min-card 2))
        (= sorted-hand-map-14-as-1 (range 1 (+ 1 (apply max sorted-hand-map-14-as-1))))
        (= (range min-card (+ 1 max-card)) sorted-hand-map)
      )
    )
  )

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
   :else 0))
