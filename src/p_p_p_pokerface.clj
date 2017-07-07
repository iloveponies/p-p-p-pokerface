(ns p-p-p-pokerface)

(def replacements {\1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7
                   \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[r _]]
  (get replacements r))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (if (some (fn [x] (> x 1)) (vals (frequencies (map rank hand))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (some (fn [x] (> x 2)) (vals (frequencies (map rank hand))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (some (fn [x] (> x 3)) (vals (frequencies (map rank hand))))
    true
    false))

(defn flush? [hand]
  (apply = (map suit hand)))

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
