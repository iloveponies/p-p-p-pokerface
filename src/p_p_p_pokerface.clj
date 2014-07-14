(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        is-digit? (Character/isDigit r)
        clothed-map {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14
                     }]
    (if is-digit?
      (Integer/valueOf (str r))
      (get clothed-map r))))

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
