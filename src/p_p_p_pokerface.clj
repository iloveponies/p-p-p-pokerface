(ns p-p-p-pokerface)


(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))



(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (not (empty? (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (not (empty? (filter (fn [x] (= x 5)) (vals (frequencies (map suit hand)))))))


(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (> (apply + (filter (fn [x] (or (= x 4) (= x 2))) (vals (frequencies (map rank hand))))) 3))

(defn high-card? [hand]
  true)

(defn straight? [hand]
  (let [x (sort (map rank hand))]
    (if (and (= (first x) 2) (contains? (set x) 14))
      (= (apply + x) 28)
      (= x (or (range (first x) (+ (first x) 5)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
