(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (Integer/valueOf (str (replacements fst))))))

(defn ranklow [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 1}]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (Integer/valueOf (str (replacements fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (= (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (full-house? hand) (four-of-a-kind? hand) (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straightlow? [hand]
  (= (range (apply min (map ranklow hand)) (+ 5 (apply min (map ranklow hand)))) (sort (map ranklow hand))))

(defn straight? [hand]
  (or (straightlow? hand) (= (range (apply min (map rank hand)) (+ 5 (apply min (map rank hand)))) (sort (map rank hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)                                                     ; All hands have a high card.

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
    (high-card? hand) 0))
