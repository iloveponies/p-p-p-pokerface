(ns p-p-p-pokerface)
;;testing
;;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])

(def values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get values fst))))

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
  (== (apply max (vals (frequencies (map suit hand)))) 5)
 )

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2,3])
  )

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (map rank hand)))) [1,2,2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lowest (first ranks)
        highest (last ranks)]
    (or
     (= ranks (range lowest (+ highest 1)))
     (= ranks [2, 3, 4, 5, 14])))
  )

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)
   )
 )

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand ) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))
