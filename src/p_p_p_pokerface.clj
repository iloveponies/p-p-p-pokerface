(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
  (def arvo {\T 10, \J 11, \Q 12, \K 13 \A 14})
    (if (Character/isDigit r)
        (Integer/valueOf (str r)) 
        (Integer/valueOf (str (arvo r))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn samoja [hand lkm]
  (contains? (set (vals (frequencies (map rank hand)))) lkm))

(defn pair? [hand]
  (samoja hand 2))

(defn three-of-a-kind? [hand]
  (samoja hand 3))

(defn four-of-a-kind? [hand]
  (samoja hand 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (samoja hand 2) (samoja hand 3)))

(defn two-pairs? [hand]
  (or (= "(2 2 1)" 
          (str (vals (frequencies (sort (map rank hand))))))
      (samoja hand 4)))

(defn straight? [hand]
  (or (= (seq (sort (map rank hand))) 
         (range (rank (get hand 0)) (+ (rank (get hand 0)) 5)))
      (= (seq (sort (replace {14 1} (map rank hand)))) 
         (range 1 (+ 1 5)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [arvio (fn [a] ((first a) hand))]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter arvio checkers))))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)












