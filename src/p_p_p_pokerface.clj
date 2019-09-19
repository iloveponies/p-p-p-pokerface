(ns p-p-p-pokerface)

(def ranks {\T 10
                \J 11
                \Q 12
                \K 13
                \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (not (empty?
        (filter #(= 2 %)
                (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty?
        (filter #(= 3 %)
                (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty?
        (filter #(= 4 %)
                (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq-values (vals (frequencies (map rank hand)))]
    (or (= 2 (count (filter #(= 2 %) freq-values)))
        (= 1 (count (filter #(= 4 %) freq-values))))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low (sort (replace {14 1} (map rank hand)))]
  (or (= ace-high
     (range (first ace-high) (+ 5 (first ace-high))))
      (= ace-low
     (range (first ace-low) (+ 5 (first ace-low)))))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))

