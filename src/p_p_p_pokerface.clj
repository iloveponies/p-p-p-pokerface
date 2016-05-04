(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [letters {\A 14,
                 \T 10,
                 \J 11,
                 \Q 12,
                 \K 13}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get letters rank))))

(defn suit [[_ suit]] 
  (str suit))

(defn pair? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 1 (count (filter (fn [[k v]] (and (= 0 (mod k 2)) (= 2 v))) freq)))))

(defn x-of-a-kind? [x hand]
  (let [freq (frequencies (map rank hand))]
    (= 1 (count (filter (fn [[_ v]] (= x v)) freq)))))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ordered (sort (map rank hand))
        first1 (first ordered)]
    (if (= 2 first1)
      (or (= '(2 3 4 5 14) ordered) (= '(2 3 4 5 6) ordered))
      (= (range first1 (+ first1 5)) ordered))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [functions {8 straight-flush?,
                   7 four-of-a-kind?,
                   6 full-house?,
                   5 flush?,
                   4 straight?
                   3 three-of-a-kind?
                   2 two-pairs?
                   1 pair?}
        hand-true (map (fn [[k _]] k) (filter (fn [[i _]] ((get functions i) hand)) functions))]
    (if (empty? hand-true)
      0
      (first hand-true))))

  
