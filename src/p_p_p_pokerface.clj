(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (str (get ranks r))))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (= [2 2 1] (vals (frequencies (map rank hand)))))

(defn straight? [hand]
  (let [a-seq (sort (map rank hand))]
    (or
       (let [low-card (apply min a-seq)]
         (= a-seq (range low-card (+ low-card 5))))
       (let [a-seq (sort(replace {14, 1} a-seq))
             low-card (apply min a-seq)]
         (= a-seq (range low-card (+ low-card 5)))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  "All hands have a high card." true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands (filter (fn  [[checker points]] (if (checker hand) points)) checkers)]
    (apply max (map second hands))))
