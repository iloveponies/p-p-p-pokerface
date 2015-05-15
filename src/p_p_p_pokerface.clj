(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [f (vals (frequencies (map rank hand)))]
   ;;; (if (filter #(= 2 %) f) true false)))
    (if (< 0 (count (filter #(= 2 %) f))) true false)))
(defn three-of-a-kind? [hand]
  (let [f (vals (frequencies (map rank hand)))]
   ;;; (if (filter #(= 2 %) f) true false)))
    (if (< 0 (count (filter #(= 3 %) f))) true false)))

(defn four-of-a-kind? [hand]
  (let [f (vals (frequencies (map rank hand)))]
    (if (< 0 (count (filter #(= 4 %) f))) true false)))

(defn flush? [hand]
  (let [f (max (vals (frequencies (map suit hand))))]
    (= 5 (first f))))

(defn full-house? [hand]
  (let [f (sort (vals (frequencies (map rank hand))))]
    (= '(2 3) f)))
(defn two-pairs? [hand]
  (let [f (sort (vals (frequencies (map rank hand))))
        a (first f)
        b (second f)]
    (or (= '(1 2 2) f) (= '(1 4) f))))


(defn straight? [hand]
  (let [a (sort (map rank hand))
        f (if (= (first a) 2) (sort (replace {14 1} a)) a)]
    (= (range (first f) (+ (last f) 1)) f))  
    )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        value? #((first %) hand)
        successes (filter value? checkers)
        values (map second successes)]
    (apply max values)))


