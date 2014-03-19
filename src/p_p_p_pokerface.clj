(ns p-p-p-pokerface)

(defn rank [card]
  (let [higher-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get higher-ranks rank)
      )
    )
  )

(defn suit [card]
  (let [[rank suite] card]
    (str suite)
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
