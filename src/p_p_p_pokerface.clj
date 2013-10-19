(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
    (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
    (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (vals (frequencies ranks)) [3 2])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (or (= (count (filter (fn [x] (= x 2)) freqs)) 2)
        (= (apply max freqs) 4))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        fst (first ranks)]
    (or (= ranks (range fst (+ fst 5)))
        (= (sort (replace {14 1} ranks)) (range 1 6)))))

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
