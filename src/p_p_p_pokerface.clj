(ns p-p-p-pokerface)

(def faces {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (faces fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (map rank hand))

(defn n-of-a-kinds [hand]
  (vals (frequencies (ranks hand))))

(defn has-n-of-a-kind? [hand n]
  (boolean (some #(= n %) (n-of-a-kinds hand))))

(defn pair? [hand]
  (has-n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (has-n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (has-n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (every? #(= (first suits) %) (rest suits))))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (= 2 (count (filter #(= 2 %) (n-of-a-kinds hand))))))

(defn straight? [hand]
  (let [ace-high       (sort (ranks hand))
        ace-low        (sort (replace {14 1} (ranks hand)))
        straight       (fn [col] (range (first col) (+ (last col) 1)))
        is-sequential? (fn [col] (= col (straight col)))]
    (or
      (is-sequential? ace-high)
      (is-sequential? ace-low))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

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
    (high-card? hand)       0))
