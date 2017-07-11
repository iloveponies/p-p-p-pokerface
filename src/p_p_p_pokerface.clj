(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
     (Integer/valueOf (str fst))
     ((fn [char] ({\T 10, \J 11, \Q 12, \K 13, \A 14} char)) fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn suitFreq [hand]
  (vals (frequencies (map suit hand))))

(defn rankFreq [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (> (apply max (rankFreq hand)) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (rankFreq hand)) 2))

(defn four-of-a-kind? [hand]
 (> (apply max (rankFreq hand)) 3))

(defn flush? [hand]
  (= 5 (apply max (suitFreq hand))))

(defn full-house? [hand]
  (= [2 3] (sort (rankFreq hand))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= [1 2 2] (sort (rankFreq hand)))))

(defn straight? [hand]
  (let [sortRanks (sort (map rank hand))
        fst (first sortRanks)]
    (or (= [2 3 4 5 14] sortRanks) (= [0 1 2 3 4] (map (fn [x] (- x fst)) sortRanks)))))

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
    :else 0
    ))
