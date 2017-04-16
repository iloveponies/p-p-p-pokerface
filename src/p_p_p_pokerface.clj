(ns p-p-p-pokerface)

; Numeric value of the card
(defn rank [card]
  (let [[value _] card
        symbols {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (and (Character/isDigit value) (< 1 (Integer/valueOf (str value)) 10)) (Integer/valueOf (str value))
      (and (not (Character/isDigit value)) (contains? symbols value)) (get symbols value)
      :else nil
      )
    )
  )

; Suit of the card as a String
(defn suit [card]
  (let [[_ sui] card]
    (str sui)
    )
  )

; Helper method to get number of same values in hand.
(defn number-of-same-values-in-hand [hand]
  (apply max (vals (frequencies (map rank hand))))
  )

(defn pair? [hand]
  (< 1 (number-of-same-values-in-hand hand))
  )

(defn three-of-a-kind? [hand]
  (< 2 (number-of-same-values-in-hand hand)))

(defn four-of-a-kind? [hand]
  (< 3 (number-of-same-values-in-hand hand)))


(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
