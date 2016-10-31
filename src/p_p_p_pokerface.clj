(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (let [extras {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (extras r)))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

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
