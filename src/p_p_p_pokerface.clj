(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\A 14, \K 13, \T 10
          \Q 12, \J 11} fst))))

(defn suit [card]
  (let [[_ snd] card]
   (str snd)))

(defn pair? [hand]
  (if (empty?
       (filter #(= 2 %)
       (vals (frequencies (map #(rank %) hand)))))
    false true))

(defn three-of-a-kind? [hand]
  (if (empty?
       (filter #(= 3 %)
       (vals (frequencies (map #(rank %) hand)))))
    false true))

(defn four-of-a-kind? [hand]
  (if (empty?
       (filter #(= 4 %)
       (vals (frequencies (map #(rank %) hand)))))
    false true))

(defn flush? [hand]
  (if (empty?
       (filter #(= 5 %)
       (vals (frequencies (map #(suit %) hand)))))
    false true))

(defn full-house? [hand]
  )

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
