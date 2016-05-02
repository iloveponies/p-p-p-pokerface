(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ranking _] card]
    (if (Character/isDigit ranking)
      (Integer/valueOf (str ranking))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} ranking))))

(defn suit [card]
  (let [[_ suite] card]
    (str suite)))

(defn pair? [hand]
  (if (>= (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (let [the-high (fn [the-hand] (apply max (vals (frequencies (map rank the-hand)))))
        three 3]
    (if (>= (the-high hand) three)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [the-high (fn [the-hand] (apply max (vals (frequencies (map rank the-hand)))))
        four 4]
    (if (>= (the-high hand) four)
      true
      false)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [the-hand (sort (vals (frequencies (map rank hand))))]
    (if (or
          (= (seq [1 4]) the-hand)
          (= (seq [1 2 2]) the-hand))
      true
      false)))

(defn straight? [hand]
  (let [ranked (fn [hand] (map rank hand))
        sorted (fn [hand] (sort (ranked hand)))
        replaced (fn [hand] (sort (replace {14 1} (ranked hand))))]
    (if (or
          (= (range (first (sorted hand)) (+ (first (sorted hand)) 5) ) (sorted hand))
          (= (range 1 6) (replaced hand))
          (= (range 10 15) (replaced hand)))
      true
      false)))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  (>= (apply max (map rank hand)) 10))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        funk (fn [x] (if ((first x) hand) (second x) 0))
        spank (map funk checkers)]
    (apply max spank)))

