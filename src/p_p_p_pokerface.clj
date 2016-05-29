(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [rank-map { \T 10, \J 11, \Q 12, \K 13, \A 14}]
    (or (get rank-map rank)
        (Integer/valueOf (str rank)))))

(defn suit [[_ suit]]
  (str suit))

(defn count-hand [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (= (max (apply max (count-hand hand)))
      2))

(defn three-of-a-kind? [hand]
  (= (max (apply max (count-hand hand)))
      3))

(defn four-of-a-kind? [hand]
  (= (max (apply max (count-hand hand)))
     4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [values (count-hand hand)]
    (if (and
          (some #{2} values)
          (some #{3} values))
      true
      false
      )))

(defn two-pairs? [hand]
  (let [values (count-hand hand)]
    (= 2 (count (filter #{2} values)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        hand-sequence (fn [x]
                        (range (first x) (+ (first x) 5)))
        replace-ace (fn [x] (sort (replace {14 1} x)))]
    (if (some #{2} ranks)
      (= (replace-ace ranks) (hand-sequence (replace-ace ranks)))
      (= ranks (hand-sequence ranks)))))

(defn straight-flush? [hand]
   (and
    (straight? hand)
    (flush? hand)))

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
    :else                   0))
