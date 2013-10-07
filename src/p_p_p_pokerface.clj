(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank] card]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     (= rank \T) 10
     (= rank \J) 11
     (= rank \Q) 12
     (= rank \K) 13
     (= rank \A) 14)))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2) true false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 3) true false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 4) true false))

(defn flush? [hand]
  (if (= (apply max (vals (frequencies (map suit hand)))) 5) true false))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (count (filter (fn [count] (= count 2)) (vals (frequencies (map rank hand))))) 2))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks2 (sort (replace {14 1} (map rank hand)))]
    (or
     (= ranks (range (first ranks) (+ (first ranks) 5)))
     (= ranks2 (range (first ranks2) (+ (first ranks2) 5))))))

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

