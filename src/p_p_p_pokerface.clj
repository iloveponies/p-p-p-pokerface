(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (mapv (fn [card] (rank card)) hand)]
    (>= (apply max (vals (frequencies ranks))) 2)))

(defn two-pairs? [hand]
  (let [ranks (mapv (fn [card] (rank card)) hand)]
    (or (= (sort (vals (frequencies ranks))) (seq [1 2 2]))
        (= (sort (vals (frequencies ranks))) (seq [2 3]))
        (= (sort (vals (frequencies ranks))) (seq [1 4])))))

(defn three-of-a-kind? [hand]
  (let [ranks (mapv (fn [card] (rank card)) hand)]
    (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (mapv (fn [card] (rank card)) hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn straight? [hand]
  (let [ranks (sort (mapv (fn [card] (rank card)) hand))]
    (or (and (== (- (apply max ranks) (apply min ranks)) 4)
             (not (pair? hand)))
        (= ranks (seq [2 3 4 5 14])))))

(defn flush? [hand]
  (let [suits (mapv (fn [card] (suit card)) hand)]
    (== (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (mapv (fn [card] (rank card)) hand)]
    (= (sort (vals (frequencies ranks))) (seq [2 3]))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else                   0))