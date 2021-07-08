(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get values r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (>=
    (apply max
            (vals
              (frequencies
                (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>=
    (apply max
            (vals
              (frequencies
                (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>=
    (apply max
            (vals
              (frequencies
                (map rank hand)))) 4))

(defn flush? [hand]
  (>=
    (apply max
            (vals
              (frequencies
                (map suit hand)))) 5))

(defn full-house? [hand]
  (=
    (sort
      (vals
        (frequencies
          (map rank hand))))
    [2 3]))

(defn two-pairs? [hand]
  (if
    (<= 4 (apply +
             (filter
               (fn [x] (>= x 2))
               (vals
                 (frequencies
                   (map rank hand))))))
    true
    false))

(defn straight? [hand]
  (let [hand (sort (map rank hand))
        hand2 (sort (replace {14 1} hand))
        check (fn [x]
                (and (apply < x)
                     (= (last x) (+ (first x) 4))))]
    (or (check hand) (check hand2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        func (fn [x] (if ((first x) hand) (second x) -1))]
    (apply max (map func checkers))))
