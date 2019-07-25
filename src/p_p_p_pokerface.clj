(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [[rank _]]
  (let [ranks {\T 10,
            \J 11,
            \Q 12,
            \K 13,
            \A 14}]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (ranks rank))))

(defn suit [[_ suit]]
  (str suit))

(defn highest-same-ranks [hand]
  (apply max
         (vals
          (frequencies
           (map rank hand)))))

(defn pair? [hand]
  (<= 2 (highest-same-ranks hand)))

(defn three-of-a-kind? [hand]
  (<= 3 (highest-same-ranks hand)))

(defn four-of-a-kind? [hand]
  (<= 4 (highest-same-ranks hand)))

(defn flush? [hand]
  (= 1
     (count
      (set
       (map suit hand)))))

(defn sorted-freqs [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (let [freqs (sorted-freqs hand)]
  (and (= 2 (first freqs))
       (= 3 (second freqs)))))

(defn two-pairs? [hand]
  (let [freqs (sorted-freqs hand)]
    (or
     (and (= 2 (second freqs))
       (= 2 (nth freqs 2)))
     (four-of-a-kind? hand)
     (full-house? hand))))

(defn straight? [hand]
  (let [ranks
        (sort
         (map rank hand))
        minrank (apply min ranks)
        ranks-low-ace
        (sort (replace {14 1}
         (map rank hand)))
        minrank-low-ace
        (apply min ranks-low-ace)]
    (or (= ranks
       (range minrank (+ minrank 5)))
        (= ranks-low-ace
       (range minrank-low-ace
              (+ minrank-low-ace 5))))))

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
