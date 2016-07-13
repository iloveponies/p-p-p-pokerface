(ns p-p-p-pokerface)

(defn int-value [raw]
  (get {\T 10, \J 11, \Q 12, \K 13, \A 14} raw))

(defn rank [card]
  (let [[value suit] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (int-value value))))

(defn suit [card]
  (let [[value suit] card]
    (str suit)))

(rank "8S")

(defn has? [seq val]
  (not (empty? (filter (fn [x] (= x val)) seq))))

(defn freq-vals [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (has? (freq-vals hand) 2))

(defn three-of-a-kind? [hand]
  (has? (freq-vals hand) 3))

(defn four-of-a-kind? [hand]
  (has? (freq-vals hand) 4))

(four-of-a-kind? ["9H" "9S" "9C" "5C" "9D"])

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(flush? ["TS" "AS" "QD" "KS" "JS"])

(defn full-house? [hand]
  (= 2 (count (frequencies (map rank hand)))))

(full-house? ["TS" "TS" "QD" "QS" "QS"])

(defn sortedRankFreqs [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (or (= '(1 2 2) (sortedRankFreqs hand)) (= '(1 4) (sortedRankFreqs hand))))

(two-pairs? ["TS" "TS" "TD" "TS" "KS"])

(sortedRankFreqs ["TS" "TS" "QD" "QS" "KS"])

(defn straightH? [hand]
  (= (sort (map rank hand)) (range (apply min (sort (map rank hand))) (+ 5 (apply min (sort (map rank hand)))))))

(defn straight? [hand]
  (or (straightH? hand) (straightH? (replace {"AH" "1H", "AD" "1D", "AC" "1C", "AS" "1S"} hand))))

(straight? ["AH" "2D" "5C" "4H" "3H"])

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
