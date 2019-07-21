(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        character-values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (character-values rnk))))

(defn suit [card]
  (let [[_ second] card]
    (str second)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= #{3 2} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= [2 2 1] (vals (frequencies (map rank hand))))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks-high-ace (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks-high-ace))]
    (and
      (= 5 (count (set ranks-low-ace)))
      (= 5 (count (set ranks-high-ace)))
      (or
       (= 4 (- (last ranks-low-ace) (first ranks-low-ace)))
       (= 4 (- (last ranks-high-ace) (first ranks-high-ace)))))))

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
