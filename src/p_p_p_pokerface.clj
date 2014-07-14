(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        is-digit? (Character/isDigit r)
        clothed-map {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14
                     }]
    (if is-digit?
      (Integer/valueOf (str r))
      (get clothed-map r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind? [n hand]
  (contains? (into #{}
                   (vals
                    (frequencies
                     (map rank hand)))
                   )
             n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and
   (pair? hand)
   (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rank-val (vals (frequencies (map rank hand)))]
    (or (four-of-a-kind? hand)
        (= 2 (count (filter #(= 2 %)
                            rank-val))))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        sorted-low-hand (sort (replace {14 1} sorted-hand))
        first-sorted-hand (first sorted-hand)
        first-sorted-low-hand (first sorted-low-hand)]
    (or (= sorted-hand (range first-sorted-hand (+ first-sorted-hand 5)))
        (= sorted-low-hand (range first-sorted-low-hand (+ first-sorted-low-hand 5))))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
