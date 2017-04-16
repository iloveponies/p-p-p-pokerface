(ns p-p-p-pokerface)

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

(defn suit [card]
  (let [[_ sui] card]
    (str sui)
    )
  )

(defn pair? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

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
