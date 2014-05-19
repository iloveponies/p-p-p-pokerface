(ns p-p-p-pokerface)

(def ranks {\2 2
            \3 3
            \4 4
            \5 5
            \6 6
            \7 7
            \8 8
            \9 9
            \T 10
            \J 11
            \Q 12
            \K 13
            \A 14})

(defn rank [[r _]]
  (ranks r))

(defn suit [[_ s]]
  (str s))

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
