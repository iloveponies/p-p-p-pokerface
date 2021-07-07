(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst) 
    (Integer/valueOf (str fst))
    (get replacements fst))
  ))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= (count (filter even? (vals (frequencies (map rank hand))))) 2)))

(defn straight? [hand]
  (let [hand-sorted (sort (map rank hand))]
    (or (= hand-sorted [2 3 4 5 14])
    (= (range (first hand-sorted) (+ (first hand-sorted) 5)) hand-sorted)
    )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))
  
