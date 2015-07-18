(ns p-p-p-pokerface)

(defn rank [card]
  (let [[a _] card]
    (if (Character/isDigit a)
      (Integer/valueOf (str a))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} a))))

(defn suit [card]
  (let [[a b] card]
    (str b)))

(defn get-hand-ranks-freq [hand]
  (vals (frequencies (map rank hand))))

(defn get-hand-suit-freq [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (let [hand-ranks-freq (get-hand-ranks-freq hand)]
    (if (< 1 (apply max hand-ranks-freq)) true false)))

(defn three-of-a-kind? [hand]
  (if (< 2 (apply max (get-hand-ranks-freq hand))) true false))

(defn four-of-a-kind? [hand]
  (if (< 3 (apply max (get-hand-ranks-freq hand))) true false))

(defn flush? [hand]
  (if (= 5 (apply max (get-hand-suit-freq hand))) true false))

(defn full-house? [hand]
  (let [ans (sort (get-hand-ranks-freq hand))
        exp (range 2 4)]
    (if (= ans exp) true false)))

(defn two-pairs? [hand]
  (let [ans (sort (get-hand-ranks-freq hand))
        exp [1 2 2]]
    (if (= ans exp) true false)))

(defn in-order [map]
  (let [exp (range (first map) (+ (first map) 5))]
    (if (= exp map) true false)))

(defn straight? [hand]
  (let [hand-ranks (map rank hand)
        ans1 (sort hand-ranks)
        ans2 (sort (replace {14 1} hand-ranks))]
    (if (or (in-order ans1) (in-order ans2)) true false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

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
