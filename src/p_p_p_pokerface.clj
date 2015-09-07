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
    (let [sorted-hand-ranks-high-ace (sort (map rank hand))
        sorted-hand-ranks-low-ace (sort (replace {14 1} (map rank hand)))
        expected-straight-high-ace (range (Integer/valueOf (first sorted-hand-ranks-high-ace)) (+ 5 (Integer/valueOf (first sorted-hand-ranks-high-ace))))
        expected-straight-low-ace (range (Integer/valueOf (first sorted-hand-ranks-low-ace)) (+ 5 (Integer/valueOf (first sorted-hand-ranks-low-ace))))
        ]
  (or (= sorted-hand-ranks-low-ace expected-straight-low-ace) (= sorted-hand-ranks-high-ace expected-straight-high-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
