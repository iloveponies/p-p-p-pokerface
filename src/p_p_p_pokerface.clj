(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\A 14, \K 13, \Q 12, \J 11, \T 10} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals(frequencies(map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals(frequencies(map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals(frequencies(map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals(frequencies(map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= [1 2 2] (sort (vals(frequencies(map rank hand)))))))

(defn straight? [hand]
  (let [ranks (sort(map rank hand))]
    (or
      (= (range 10 15) ranks)
      (= (range 9 14) ranks)
      (= (range 8 13) ranks)
      (= (range 7 12) ranks)
      (= (range 6 11) ranks)
      (= (range 5 10) ranks)
      (= (range 4 9) ranks)
      (= (range 3 8) ranks)
      (= (range 2 7) ranks)
      (= (range 1 6) (sort(replace {14 1}ranks))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
