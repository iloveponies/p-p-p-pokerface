(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card
        replacement {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacement r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (map rank hand)))
        pairs (filter (fn [x] (= 2 x)) ranks)]
    (= 2 (count pairs))))

(defn seqiential? [col]
  (let [col (sort col)
        f (first col)]
    (= col (range f (+ f 5)))))

(defn seqiential-hand? [hand]
  (let [ranks (map rank hand)]
    (or (seqiential? ranks)
        (seqiential? (replace {14 1} ranks)))))

(defn straight? [hand]
  (let [suits (set (map suit hand))]
    (and (>= (count suits) 2)
         (seqiential-hand? hand))))

(defn straight-flush? [hand]
  (let [suits (set (map suit hand))]
    (and (= (count suits) 1)
         (seqiential-hand? hand))))

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
