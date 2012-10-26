(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rankchr _] card]
    (if (Character/isDigit rankchr)
      (Integer/valueOf (str rankchr))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rankchr))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (let [nranks (sort (vals (frequencies (map rank hand))))]
    (or (= nranks [1 2 2]) (= nranks [2 3]) (= nranks [1 4]))))

(defn straight? [hand]
  (let [hlpr (fn [r] (= r (range (first r) (+ (first r) 5))))
        ranks (sort (keys (frequencies (map rank hand))))]
    (boolean (or (hlpr ranks) (hlpr (sort (replace {14 1} ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))