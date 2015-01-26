(ns p-p-p-pokerface)

(defn rank [card]
  (let [facecards {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (let [[rank _] card]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get facecards rank)))))

(defn rank-freqs [hand]
  (vals (frequencies (map rank hand))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (not (nil? (some #(= % 2) (rank-freqs hand))))))

(defn two-pairs? [hand]
  (= (count (filter (fn [elem] (= elem 2)) (rank-freqs hand)))2))

  (defn straight? [hand]
    (let [ranks (sort (map rank hand))]
      (let [change-to-low (fn []  (sort (seq (assoc (into[] ranks ) 4 1))))]
        (let [make-straight (fn [start] (range start (+ start 5)))]
          (if (and (= (apply max ranks) 14) (< (apply min ranks) 10))
            (= (make-straight (first (change-to-low))) (change-to-low))
            (= (make-straight (first ranks)) ranks))))
    ))

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
