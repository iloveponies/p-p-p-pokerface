(ns p-p-p-pokerface)

(defn rank [[rank _]]
  ({\2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9
    \T 10
    \J 11
    \Q 12
    \K 13
    \A 14} rank))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= #{2 3} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [[r1 r2 _ _ r5 :as ranks] (vec (sort (map rank hand)))
        [r1 :as ranks] (if (and (= r1 2) (= r5 14))
                         (cons 1 (subvec ranks 0 4))
                         ranks)]
    (= ranks (range r1 (+ 5 r1)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
