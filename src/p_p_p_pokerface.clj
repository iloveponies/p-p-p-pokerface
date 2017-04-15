(ns p-p-p-pokerface)

(def arvot {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[fir _] card]
    (if (Character/isDigit fir)
      (Integer/valueOf (str fir))
      (arvot fir))))


(defn suit [card]
  (let [[_ sdn] card]
    (str sdn)))


(defn pair? [hand]
 (>= (apply max (vals (frequencies (map rank hand)))) 2))


(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))


(defn two-pairs? [hand]
  (or (= (sort  (vals (frequencies (map rank hand)))) (seq [1 2 2])) (four-of-a-kind? hand)))



(defn straight? [hand]
  (let [arvot (seq (sort (map rank hand)))
        vertaa (range (first arvot) (+ 5 (first arvot)))
        vertaa2  ( sort(cons 14 (range 2 6)))]
    (or (= vertaa arvot) (= vertaa2 arvot))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


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
   :else 0

   )
)

