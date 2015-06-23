(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})


(defn rank [card]
  (let [value (first card)]
  (cond
   (Character/isDigit value) (Integer/valueOf (str value))
   :else (replacements value))
   )
  )

(defn suit [card]
  (str(second card))
  )

(defn pair? [hand]
  (cond
    (some #(= 2 %) (vals (frequencies (map rank hand)))) true
   :else false
   )
  )

(defn three-of-a-kind? [hand]
  (cond
    (some #(= 3 %) (vals (frequencies (map rank hand)))) true
     :else false
   )
  )

(defn four-of-a-kind? [hand]
  (cond
    (some #(= 4 %) (vals (frequencies (map rank hand)))) true
   :else false
   )
  )

(defn flush? [hand]
  (= (first(vals(frequencies (map suit hand)))) 5)
  )

;(defn full-house? [hand]
;  (let [[a b] (vals (frequencies (map rank hand)))]
;    (== (+ a b) 5)
;    ))

(defn full-house? [hand]
  (cond
   (= (pair? hand) (three-of-a-kind? hand) true) true
   :else false
   )
  )

(defn two-pairs? [hand]
  (= (count (filter (fn [x] (== x 2)) (vals (frequencies (map rank hand))))) 2)
  )

(defn straight? [hand]
  (let [
        x (sort (map rank hand))
        y (replace {14 1}(sort (map rank hand)))
        maxx (apply max x)
        minx (apply min x)
        miny (apply min y)
        maxy (apply max y)]
    (and (= (pair? hand) (three-of-a-kind? hand) (four-of-a-kind? hand) false) (or (== (- maxx minx) 4) (== (- maxy miny) 4)))
  )
  )

(defn straight-flush? [hand]
   (= (straight? hand) (flush? hand) true)
  )

(defn high-card? [hand]
  ;;(apply max (map rank hand))
  true
  )

(defn value [hand]
  (let [check #{
                [high-card? 0]
                [pair? 1]
                [two-pairs? 2]
                [three-of-a-kind? 3]
                [straight? 4]
                [flush? 5]
                [full-house? 6]
                [four-of-a-kind? 7]
                [straight-flush? 8]
                }

     result (apply max(map (fn[x] (cond (= ((first x) hand) true) (second x) :else 0)) check))
    ]
    result
    )
  )


