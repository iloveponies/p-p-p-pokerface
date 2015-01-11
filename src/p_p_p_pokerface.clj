(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies(map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies(map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies(map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies(map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (cond
   (four-of-a-kind? hand) true
   (= [1 2 2] (sort (vals (frequencies (map rank hand))))) true
   :else false))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min-rank (first ranks)
        alternative-ranks (sort (replace {14 1} ranks))
        min-alternative (first alternative-ranks)]
    (cond
      (= ranks (range min-rank (+ min-rank 5))) true
      (= alternative-ranks (range min-alternative (+ min-alternative 5))) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches (filter (fn [x] ((first x) hand)) checkers)
        values (map second matches)]
    (apply max values)))
