(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [a (get card 0)]
    (if (Character/isDigit a) (Integer/valueOf (str a)) (replacements a))))

(defn suit [card]
  (str (get card 1)))

(defn ranks [hand]
    (map rank hand))

(defn suits [hand]
    (map suit hand))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (ranks hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (ranks hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (ranks hand))))))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (ranks hand)))) [2,3]))

(defn two-pairs? [hand]
  (or (full-house? hand) (four-of-a-kind? hand)
    (= (sort (vals (frequencies (ranks hand)))) [1,2,2])))

(defn straight? [hand]
  (let [min (apply min (ranks hand))]
    (or (= (sort (ranks hand)) (range min (+ min 5)))
        (= (sort (ranks hand)) [2,3,4,5,14]))))

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
