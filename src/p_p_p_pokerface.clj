(ns p-p-p-pokerface)

(defn rank [card]
  "Exercise 2"
  (def the-big-ones {\A 14, \K 13 \Q 12 \J 11 \T 10})
  (let [[rank _] card]
    (let [digit (Character/isDigit rank)]
      (if digit
        (Integer/valueOf (str rank))
        (the-big-ones rank)))))


(defn suit [card]
  "Exercise 1"
  (let [[_ suit] card]
    (str suit)))

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
