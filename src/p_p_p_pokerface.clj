(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13,\A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)
    )))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
  (> (apply max(vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
   (> (apply max(vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max(vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (let [color (suit (first hand))]
    (= (count (filter (fn[x] (= (suit x) color)) hand)) 5)
    ))

(defn full-house? [hand]
  (let [val (sort (vals (frequencies (map rank hand))))]
    (= val [2 3])
  ))

(defn two-pairs? [hand]
   (let [val (sort (vals (frequencies (map rank hand))))]
    (or (= val [1 2 2]) (= val [1 4]) (= val [2 3]))
  ))

(defn straight? [hand]
  (let [h (sort (map rank hand))]
     (let [min-el (apply min h)
           low-ace (= h [2 3 4 5 14])
           high-ace (= h (seq (range min-el (+ min-el 5) )))]
       (if (= (apply max h) 14) (or low-ace high-ace) high-ace ))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
   (apply max (map second (filter (fn[x] ((first x) hand) ) checkers)))))
