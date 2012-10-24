(ns p-p-p-pokerface)

(defn in? [lst elem]
  (not (empty? (filter (fn [x] (= x elem)) lst))))

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
       [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ranks [hand]
  (map rank hand))

<<<<<<< HEAD
(defn suits [hand]
  (map suit hand))

(defn freqs [hand]
  (vals (frequencies (ranks hand))))

(defn pair? [hand]
  (in? (freqs hand) 2))

=======
>>>>>>> FETCH_HEAD
(defn three-of-a-kind? [hand]
  (in? (freqs hand) 3))

(defn four-of-a-kind? [hand]
  (in? (freqs hand) 4))

(defn two-pairs? [hand]
    (or
        (= (get (frequencies (freqs hand)) 2) 2)
        (four-of-a-kind? hand)))

<<<<<<< HEAD
(defn straight? [hand]
  (let [check (fn [rnks]
                (and
                     (== (count (frequencies rnks)) 5)
                     (== (- (apply max rnks) (apply min rnks)) 4)))
        rnks (ranks hand)]
    (or (check rnks) (check (replace {14 1} rnks)))))

=======
>>>>>>> FETCH_HEAD
(defn flush? [hand]
  (let [sts (suits hand)]
    (every? (fn [s] (= s (first sts))) (rest sts))))

(defn full-house? [hand]
  (and
       (in? (freqs hand) 2)
       (in? (freqs hand) 3)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  (and
       (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind?  hand) 7
    (full-house?      hand) 6
    (flush?           hand) 5
    (straight?        hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs?       hand) 2
    (pair?            hand) 1
    :else 0))
