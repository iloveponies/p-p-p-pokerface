(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rankchar _] card]
    (if (Character/isDigit rankchar)
      (Integer/valueOf (str rankchar))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rankchar))))

(defn suit [card]
  (let [[_ suitchar] card]
    (str suitchar)))

(defn pair? [hand]
  (let [ranksofhand (map (fn [card] (rank card)) hand)]
    (if (>= (apply max (vals (frequencies ranksofhand))) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranksofhand (map (fn [card] (rank card)) hand)]
    (if (>= (apply max (vals (frequencies ranksofhand))) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranksofhand (map (fn [card] (rank card)) hand)]
    (if (>= (apply max (vals (frequencies ranksofhand))) 4)
      true
      false)))

(defn flush? [hand]
  (let [suitsofhand (map (fn [card] (suit card)) hand)]
    (if (== (apply max (vals (frequencies suitsofhand))) 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranksofhand (map (fn [card] (rank card)) hand)]
    (if (and
         (== (apply max (vals (frequencies ranksofhand))) 3)
         (== (apply min (vals (frequencies ranksofhand))) 2))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranksofhand (map (fn [card] (rank card)) hand)]
     (if (= (get (frequencies (vals (frequencies ranksofhand))) 2) 2)
       true
       false)))

(defn straight? [hand]
  (let [ranksofhand (sort (map (fn [card] (rank card)) hand))
        ranksofhand-low-ace (sort (replace {14 1} (map (fn [card] (rank card)) hand)))
        minrank (apply min ranksofhand)
        minrank-low-ace (apply min ranksofhand-low-ace)
        ]
    (if (or
         (= ranksofhand (range minrank (+ minrank 5)))
         (= ranksofhand-low-ace (range minrank-low-ace (+ minrank-low-ace 5))))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0
  ))
