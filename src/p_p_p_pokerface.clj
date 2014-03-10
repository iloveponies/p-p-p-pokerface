(ns p-p-p-pokerface)

(def rank-chars "23456789TJQKA")
(def ranks (zipmap rank-chars (range 2 15)))

(defn rank [[rank-char _]]
  (ranks rank-char))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (some #(= % 2) (vals (frequencies hand))))

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
