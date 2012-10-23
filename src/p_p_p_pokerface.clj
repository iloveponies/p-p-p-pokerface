(ns p-p-p-pokerface)

(defn suit [[_ s]]
  (str s))

(defn rank [card]
  (let[[s _] card
       r {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if(contains? r s)
      (get r s)
      (Integer/valueOf (str s)))))

(defn pair? [hand]
    (if( < 1 (apply max (vals(frequencies(map rank hand)))))
      true
      false))

(defn two-pairs? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)