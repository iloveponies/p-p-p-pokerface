(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        vals {\T 10,
              \J 11,
              \Q 12,
              \K 13,
              \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get vals r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn gen-freqs [hand func]
  (vals (frequencies (map func hand))))

(defn rank-freqs [hand]
  (gen-freqs hand rank))

(defn suit-freqs [hand]
  (gen-freqs hand suit))

(defn number-of-cards-of-same-rank? [hand number]
  (contains? (set (rank-freqs hand))
             number))

(defn number-of-cards-of-same-suit? [hand number]
  (contains? (set (suit-freqs hand))
             number))

(defn pair? [hand]
  (number-of-cards-of-same-rank? hand 2))

(defn three-of-a-kind? [hand]
  (number-of-cards-of-same-rank? hand 3))

(defn four-of-a-kind? [hand]
  (number-of-cards-of-same-rank? hand 4))

(defn flush? [hand]
  (number-of-cards-of-same-suit? hand 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (or
   (= 2 (count (filter (fn [n] (= 2 n)) (rank-freqs hand))))
   (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high-card (apply max ranks)
        low-card (apply min ranks)
        is-a-straight? (fn [hand-to-check high low] (= (sort hand-to-check) (range low (inc high))))]
    (if (not= high-card 14)
      (is-a-straight? ranks high-card low-card)
    (let [ace-high ranks
          ace-low (replace {14 1} ranks)
          new-high (apply max ace-low)]
      (or (is-a-straight? ace-high high-card low-card)
          (is-a-straight? ace-low new-high 1))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
