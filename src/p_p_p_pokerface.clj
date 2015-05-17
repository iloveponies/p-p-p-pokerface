(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
   (if (Character/isDigit rank) (Integer/valueOf (str rank)) 
       (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
   (str suit)))

(defn pair? [hand]
  (let [vals (vals (frequencies (map rank hand)))] do 
  (if (and (== (apply max vals) 2) (== (count vals) 4)) true false)))

(defn three-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))] do
  (if (and (== (apply max vals) 3) (== (count vals) 3)) true false)))

(defn four-of-a-kind? [hand]
  (let [vals (vals (frequencies (map rank hand)))] do
  (if (and (== (apply max vals) 4) (== (count vals) 2)) true false)))

(defn flush? [hand]
  (let [vals (vals (frequencies (map suit hand)))] do
  (if (== (apply max vals) 5) true false)))

(defn full-house? [hand]
  (let [vals (vals (frequencies (map rank hand)))] do
  (if (and (== (apply max vals) 3) (== (count vals) 2)) true false)))

(defn two-pairs? [hand]
  (let [vals (vals (frequencies (map rank hand)))] do
  (if (and (== (apply max vals) 2) (== (count vals) 3)) true false)))

(defn straight? [hand]
  (let [cards (sort (map rank hand))
	min (apply min cards)] do 
  (if (or (= cards (range min (+ min 5))) (= (sort (replace {14 1} cards)) 
          (range 1 6))) true false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[(high-card? hand) 0] [(pair? hand) 1] [(two-pairs? hand) 2] 
       [(three-of-a-kind? hand) 3] [(straight? hand) 4] [(flush? hand) 5] 
       [(full-house? hand) 6] [(four-of-a-kind? hand) 7] 
       [(straight-flush? hand) 8]}] do 
  (apply max (map second (filter (fn [match] (first match)) checkers)))))

