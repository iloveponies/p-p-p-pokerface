(ns p-p-p-pokerface)

(defn rank [card]
  (def numit {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (numit fst)))))
  )

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [hand (map rank hand)]
    (contains? (set (vals (frequencies hand))) 2)
    ))

(defn three-of-a-kind? [hand]
  (let [hand (map rank hand)]
    (contains? (set (vals (frequencies hand))) 3)
    ))

(defn four-of-a-kind? [hand]
  (let [hand (map rank hand)]
    (contains? (set (vals (frequencies hand))) 4)
    ))

(defn flush? [hand]
  (let [hand (map suit hand)]
    (contains? (set (vals (frequencies hand))) 5)
    ))

(defn full-house? [hand]
  (let [hand (map rank hand)]
   (and (contains? (set (vals (frequencies hand))) 2) (contains? (set (vals (frequencies hand))) 3))
    ))

(defn two-pairs? [hand]
  (let [hand (map rank hand)]
    (or (= (sort (vals (frequencies hand))) [1 2 2]) (= (sort (vals (frequencies hand))) [1 4]))
    ))

(defn straight? [hand]
  (let [hand (map rank hand)]
    (or (= (sort hand) (range (first hand) (+ (first hand) 5))) (= (sort hand) [2 3 4 5 14]))
    )
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  nil)