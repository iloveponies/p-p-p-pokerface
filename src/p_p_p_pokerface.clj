(ns p-p-p-pokerface)

(def ranks [1 2 3 4 5 6 7 8 9 10 11 12 13 14])
(def suits ["J" "Q" "K" "A"])

(defn rank [[n s]]
  (if (Character/isDigit n)
    (Integer/valueOf (str n))
    (get {\J 11, \Q 12, \K 13, \A 14, \T 10} n)))

(defn suit [[n s]]
  (str s))

(defn n-of-a-kind? [n hand]
  (>=
    (apply max
           (vals (frequencies (map rank hand))))
    n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (==
    (apply max
           (vals (frequencies (map suit hand))))
    5))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and
      (== 2 (count freqs))
      (== 3 (apply max freqs)))))

(defn two-pairs? [hand]
  (==
    (count (filter (partial == 2)
                   (vals (frequencies (map rank hand)))))
    2))

(defn straight? [hand]
  (let [rhand (map rank hand)
        rephand (replace {14 1} rhand)
        mhand (apply min rhand)
        mrephand (apply min rephand)]
    (or
      (= (sort rhand)
         (range mhand (+ mhand 5)))
      (= (sort rephand)
         (range mrephand (+ mrephand 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else 0))
