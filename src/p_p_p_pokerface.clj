(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn same-ranks [hand]
  (vals (frequencies (ranks hand))))

(defn same-suits [hand]
  (vals (frequencies (suits hand))))

(defn pair? [hand]
  (== (apply max (same-ranks hand)) 2))

(defn three-of-a-kind? [hand]
  (if (== (apply max (same-ranks hand)) 3)
    (if (== (apply min (same-ranks hand)) 2) false true)
    false))

(defn four-of-a-kind? [hand]
  (== (apply max (same-ranks hand)) 4))

(defn flush? [hand]
  (== (count (same-suits hand)) 1))

(defn full-house? [hand]
  (if (== (apply max (same-ranks hand)) 3)
    (if (== (apply min (same-ranks hand)) 2) true false)
    false))

(defn two-pairs? [hand]
  (= (sort (same-ranks hand)) (seq [1 2 2])))

(defn straight? [hand]
  (let [new-hand (replace {"AH" "1H", "AC" "1C", "AD" "1D", "AS" "1S"} hand)
        lowest (apply min (ranks hand))
        new-lowest (apply min (ranks new-hand))]
      (or
       (= (sort (ranks hand)) (range lowest (+ lowest 5)))
       (= (sort (ranks new-hand)) (range new-lowest (+ new-lowest 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (if (or (straight? hand) (flush? hand))
    false
    (== (apply max (same-ranks hand)) 1)))

(defn value [hand]
  nil)
