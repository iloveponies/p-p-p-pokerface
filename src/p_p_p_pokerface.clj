(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card
        rank fst]
    (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else (get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst)
     )))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set(vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set(vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set(vals (frequencies (map rank hand)))) 4))


(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (get (frequencies(vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
   (let [sortedHand (sort (map rank hand))
         lowAceSorted(sort (replace {14, 1} (map rank hand)))]
     (or (= (range (apply min sortedHand) (+ (apply max sortedHand) 1) ) sortedHand)
         (= (range (apply min lowAceSorted) (+ (apply max lowAceSorted) 1) ) lowAceSorted))))

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
