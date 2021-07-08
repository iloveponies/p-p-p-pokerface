(ns p-p-p-pokerface)

(def values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (values fst)))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn freqs [hand]
  (set(vals (frequencies (ranks hand)))))

(defn pair? [hand]
  (contains? (freqs hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (freqs hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (freqs hand) 4))

(defn flush? [hand]
  (apply = (set(map suit hand))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (= (count(filter (fn [value] (= value 2)) (vals(frequencies (ranks hand))))) 2)
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (if (contains? (set(ranks hand)) 14)
    (or                                                   ;; if there's an ace, it's a straight if
      (= (sort (ranks hand)) (range 10 15))          ;; a) it's a range from 10 to 14, or
      (= (take 4 (sort (ranks hand))) (range 2 6)))  ;; b) it's a range from 2 to 5 plus the ace
    (=                                                    ;; Otherwise, check that sorted hand is equal to range(min hand, max hand)
      (sort (ranks hand))
      (range
        (apply min (ranks hand))
        (+ 1 (apply max (ranks hand)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max
    (map (fn [test]
      (if ((first test) hand)
          (second test)
          0))
      checkers ))))