(ns p-p-p-pokerface)

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (get replacements card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn pair? [hand]
  (let [two? (fn [elem] (= elem 2))]
    (= (count (filter two? (vals (frequencies (map rank hand))))) 1)))

(defn three-of-a-kind? [hand]
  (let [three? (fn [elem] (= elem 3))]
    (= (count (filter three? (vals (frequencies (map rank hand))))) 1)))

(defn four-of-a-kind? [hand]
  (let [four? (fn [elem] (= elem 4))]
    (= (count (filter four? (vals (frequencies (map rank hand))))) 1)))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [two? (fn [elem] (= elem 2))]
    (or (= (count (filter two? (vals (frequencies (map rank hand))))) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        replaced (if (= (first sorted) 2)
                   (replace {14 1} sorted)
                   sorted)
        new-sorted (sort replaced)
        lower-bound (apply min new-sorted)
        upper-bound (apply max new-sorted)
        target (range lower-bound (+ upper-bound 1))]
    (= new-sorted target)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
      (let [checkers #{[high-card? 0]  [pair? 1]
                       [two-pairs? 2]  [three-of-a-kind? 3]
                       [straight? 4]   [flush? 5]
                       [full-house? 6] [four-of-a-kind? 7]
                       [straight-flush? 8]}

            score-check (fn [elem] (if ((first elem) hand) (second elem) 0))
            scores (map score-check checkers)]
      (apply max scores)))
