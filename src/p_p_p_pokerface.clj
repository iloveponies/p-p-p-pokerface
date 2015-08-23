(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11 \Q 12 \K 13 \A 14})
  (let [[fst snd] card]
  (if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacements fst))))

(defn suit [card]
  (let [[fst snd] card]
  (str snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 2 2]))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
