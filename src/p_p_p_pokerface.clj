(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[r _]]
  (if
    (Character/isDigit r)
    (Integer/valueOf (str r))
    (replacements r)))

(defn suit [[_ p-suit]]
  (str p-suit))

(defn pair? [hand]
  (=
    (apply max
           (vals
             (frequencies
               (map rank hand))))

    2))

(defn three-of-a-kind? [hand]
  (=
    (apply max
           (vals
             (frequencies
               (map rank hand))))

    3))

(defn four-of-a-kind? [hand]
  (=
    (apply max
           (vals
             (frequencies
               (map rank hand))))

    4))

(defn flush? [hand]
  (= (count
       (keys
         (frequencies
           (map suit hand))))
     1))

(defn full-house? [hand]
  (=
    (sort
      (vals
        (frequencies
          (map rank hand))))
    [2, 3]
    ))

(defn two-pairs? [hand]
  (if
    (four-of-a-kind? hand)
    true
    (=
      (sort
        (vals
          (frequencies
            (map rank hand))))
      [1 2 2]
      )))

(defn straight? [hand]
  (let [
        cards (sort (map rank hand))
        ]
    (if
      (< (first cards) 10)
      (let [arr (sort
                  (replace {14 1} cards))]
        (= (range
             (first arr)
             (inc
               (last arr)))
           arr))
      (=
        (range
          (first cards)
          (inc
            (last cards)))
        cards)
      )))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply
      max
      (flatten
        (filter
          (fn [[res _]] res)
          (map
            (fn [[func value]]
              (let [result (func hand)]
                (if result [value])
                ))
            checkers))))))

