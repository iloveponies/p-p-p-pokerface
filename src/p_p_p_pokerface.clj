(ns p-p-p-pokerface)


(defn rank [card]
  (let [[fst _] card
       replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn atLeastN [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (atLeastN hand 2))


(defn three-of-a-kind? [hand]
  (atLeastN hand 3))

(defn four-of-a-kind? [hand]
  (atLeastN hand 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn rankHand [hand]
  (seq (sort (vals (frequencies (map rank hand))))))

(defn rankKeyHand [hand]
  (seq (sort (keys (frequencies (map rank hand))))))

(defn full-house? [hand]
  (= [2 3] (rankHand hand)))

(defn two-pairs? [hand]
  (or
   (atLeastN hand 4)
   (= [2 3] (rankHand hand))
   (= [1 2 2] (rankHand hand))))


(defn straight? [hand]
  (let [ranks (rankKeyHand hand)
        sRanks (sort (replace {14 1} ranks))]
    (and
     (= (count ranks) 5)
     (or
      (= (range (apply min ranks) (+ (apply max ranks) 1)) ranks)
      (= (range (apply min sRanks) (+ (apply max sRanks) 1)) sRanks)))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))
