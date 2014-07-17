(ns p-p-p-pokerface)

(defn rank [card]
  nil)


;; takes a singe card and returns the suit of the card as a one character string.

(defn suit [card]
  (let [[_ s] card]
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
