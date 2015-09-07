(ns p-p-p-pokerface)

(defn rank [card]
  (let [[card-value _] card]
    (def high-cards {\T 10, \J 11, \Q 12, \K 13, \A 14})

    (if (Character/isDigit card-value) 
      (Integer/valueOf (str card-value))
      (Integer/valueOf (str (high-cards card-value))))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn rank-frequency [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (== (rank-frequency hand) 2))

(defn three-of-a-kind? [hand]
  (== (rank-frequency hand) 3))

(defn four-of-a-kind? [hand]
  (== (rank-frequency hand) 4))

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
