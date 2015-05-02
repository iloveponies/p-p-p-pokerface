(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=
   (sort (seq (vals (frequencies (map rank hand)))))
   (seq [2 3])))

(defn two-pairs? [hand]
  (=
   (sort (seq (vals (frequencies (map rank hand)))))
   (seq [1 2 2])))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        lowest (first sorted-hand)
        ace-as-lowest (sort (replace {14 1} sorted-hand))
        ace-smallest  1]
    (or
      (= sorted-hand (range lowest (+ lowest 5)))
      (= ace-as-lowest (range 1 6)))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-key (fn [checker] (first checker))
        checker-val (fn [checker] (second checker))
        hand-checker-OK? (fn [checker] ((checker-key checker) hand))
        OK-checkers (filter hand-checker-OK? checkers)
        vales (map checker-val OK-checkers)]
 (apply max vales)))
