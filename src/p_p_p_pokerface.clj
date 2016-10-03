(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (Integer/valueOf (str (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank)))))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (if (some #{2} (vals (frequencies (map rank hand))))
    true
    false))


(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))


(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (and
      (pair? (vec (take 3 (sort hand))))
      (pair? (vec (take-last 3 (sort hand)))))
    (four-of-a-kind? hand)
    (full-house? hand)))

(defn straight? [hand]
  (letfn [(rank-sorter [hand] (sort (map rank hand)))
          (result [hand]
                  (and
                    (apply < hand)
                    (= 4 (- (last hand) (first hand)))))]
    (or
      (result (rank-sorter hand))
      (result (sort (replace {14 1} (rank-sorter hand)))))))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

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
