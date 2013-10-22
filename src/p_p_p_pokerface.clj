(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        mapah {\T 10, \J 11, \Q 12, \K 13, \A 14}
        toInt (fn [x] (Integer/valueOf (str x)))]
    (if (not(Character/isDigit rank)) (mapah rank) (toInt rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- handRankFreq [hand]
  (frequencies (map rank hand)))

(defn- handSuitFreq [hand]
  (frequencies (map suit hand)))

(defn- maxSameValue [value hand]
  (let [func (fn [x] (== x value))]
          (== 1 (count (filter func (vals (handRankFreq hand)))))))

(defn pair? [hand]
  (maxSameValue 2 hand))

(defn- real-two-pairs? [hand]
  (let [func (fn [x] (== 2 x))]
    (== 2 (count (filter func (vals (handRankFreq hand)))))))

(defn two-pairs? [hand]
  (or (real-two-pairs? hand) (maxSameValue 4 hand)))

(defn three-of-a-kind? [hand]
  (maxSameValue 3 hand))

(defn four-of-a-kind? [hand]
  (maxSameValue 4 hand))

(defn straight? [hand]
  (let [shvalues (sort (map rank hand))
        smvalues (sort (replace {14 1} shvalues))
        [minv _ _ _ highv] shvalues
        minVals (range (- minv 1) (+ minv 4))
        maxVals (range (- highv 4) (+ highv 1))]
    (or (= maxVals shvalues) (= minVals smvalues))))

(defn flush? [hand]
  (== 1 (count (handSuitFreq hand))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn hand-has-value? [hand value]
  (let [checkers [(fn [_] true) pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
    ((checkers value) hand)))

(defn value [hand]
  (let [values [0 1 2 3 4 5 6 7 8]
        func (fn [x] (if (hand-has-value? hand x) x -1))]
    (apply max (map func values))))
