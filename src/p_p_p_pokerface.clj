(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rankchar _] card
        is-digit (Character/isDigit rankchar)]
    (if is-digit (Integer/valueOf (str rankchar)) (replacements rankchar))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequency-values [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (contains? (set (frequency-values hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (frequency-values hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (frequency-values hand)) 4))

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

