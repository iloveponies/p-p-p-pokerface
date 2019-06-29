(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (let
        [rankMap {\T 10, \J 11, \Q 12, \K 13, \A 14 }]
        (get rankMap rank)))));

(defn suit [card]
  (let
    [[rank suit] card]
    (str suit)));



(defn rankFrequencies [hand]
  (frequencies (map rank hand)))

(defn rankFrequencyVals [hand]
  (vals (frequencies (map rank hand))))

(defn maxRankFrequency [hand]
  (apply max (rankFrequencyVals hand)))

(defn rankFrequency [hand _rank]
  (let [freqs (rankFrequencies hand)]
    (or (get freqs _rank) 0)))


(defn suitFrequencies [hand]
  (frequencies (map suit hand)))

(defn suitFrequencyVals [hand]
  (vals (frequencies (map suit hand))))

(defn maxSuitFrequency [hand]
  (apply max (suitFrequencyVals hand)))

(defn suitFrequency [hand _suit]
  (let [freqs (suitFrequencies hand)]
    (or (get freqs _suit) 0)))




(defn pair? [hand]
  (>= (maxRankFrequency hand) 2))

(defn three-of-a-kind? [hand]
  (>= (maxRankFrequency hand) 3))

(defn four-of-a-kind? [hand]
  (>= (maxRankFrequency hand) 4))

(defn flush? [hand]
  (== 5 (maxSuitFrequency hand)))

(defn full-house? [hand]
  (let [rankFreqVals (rankFrequencyVals hand)]
    (= [3, 2] (sort > rankFreqVals))))

(defn two-pairs? [hand]
   (let [rankFreqVals (rankFrequencyVals hand)]
    (= [2, 2, 1] (sort > rankFreqVals))))

(defn straight? [hand]
   (let
     [rankValues (sort < (map rank hand))
      isStraightInner (fn [ranks] ( let [[lowestCard] ranks
                                          result (apply str (replace { lowestCard "a", (+ 1 lowestCard ) "b", (+ 2 lowestCard) "c", (+ 3 lowestCard) "d", (+ 4 lowestCard) "e" } ranks))]
                                     (= "abcde" result)))]

     (or (isStraightInner rankValues) (isStraightInner (sort < (replace { 14 1 } rankValues))))))



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
