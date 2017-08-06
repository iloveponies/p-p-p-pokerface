(ns p-p-p-pokerface)

(defn rank [card]
  (let [[a _] card]
    (if (Character/isDigit a)
      (Integer/valueOf (str a))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} a))))

(defn suit [card]
  (let [[_ m] card]
        (str m)))

(defn pair? [hand]
  (boolean (== (apply max (vals (frequencies (map rank hand)))) 2)))

(defn three-of-a-kind? [hand]
  (boolean (== (apply max (vals (frequencies (map rank hand)))) 3)))

(defn four-of-a-kind? [hand]
  (boolean (== (apply max (vals (frequencies (map rank hand)))) 4)))

(defn flush? [hand]
  (boolean (== (apply max (vals (frequencies (map suit hand)))) 5)))

(defn full-house? [hand]
  (boolean (and (some #{3} (vals (frequencies (map rank hand))))
                (some #{2} (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (boolean (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
               (= [1 4] (sort (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [kasi (map rank hand)]
    (boolean (or (= [2 3 4 5 14] (sort kasi))
                 (= [2 3 4 5 6] (sort kasi))
                 (= [3 4 5 6 7] (sort kasi))
                 (= [4 5 6 7 8] (sort kasi))
                 (= [5 6 7 8 9] (sort kasi))
                 (= [6 7 8 9 10] (sort kasi))
                 (= [7 8 9 10 11] (sort kasi))
                 (= [8 9 10 11 12] (sort kasi))
                 (= [9 10 11 12 13] (sort kasi))
                 (= [10 11 12 13 14] (sort kasi))
                 (= [1 10 11 12 13] (sort kasi))))))


(defn straight-flush? [hand]
  (boolean (and (straight? hand) (flush? hand))))

(defn high-card? [hand]
  true)

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
