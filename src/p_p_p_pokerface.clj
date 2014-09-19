(ns p-p-p-pokerface)


(defn suit [card]
  (let [[_ s] card]
    (str s)))

(def face-value-map
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[f _] card]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (get face-value-map f))))


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
