(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rank-map {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rank-map (str fst)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))



(defn pair? [hand]
 (if (empty? (filter (fn [x] (= x 2)) (vals(frequencies(map rank hand)))))
  false
  true
  ))

(defn three-of-a-kind? [hand]
 (if (empty? (filter (fn [x] (= x 3)) (vals(frequencies(map rank hand)))))
  false
  true
  ))


(defn four-of-a-kind? [hand]
 (if (empty? (filter (fn [x] (= x 4)) (vals(frequencies(map rank hand)))))
  false
  true
  ))

(defn flush? [hand]
  (apply = (map suit hand)))


(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= x 2)) (vals(frequencies(map rank hand)))))))


(defn straight? [hand]
  (let [values (map rank hand)
        straight-high (sort values)
        straight-low (sort (replace {14 1} straight-high))]
  (or
    (= straight-high (range (apply min values) (+ (apply max values) 1)))
    (= straight-low (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
