(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\A 14, \K 13 \Q 12 \J 11 \T 10} fst))))

(defn suit [card]
  (let [[_ sn] card]
  (str sn)))

(defn pair? [hand]
  (contains? (set (mapv (fn [x] (= 2 x)) (vals (frequencies (mapv rank hand))))) true))

(defn three-of-a-kind? [hand]
 (contains? (set (mapv (fn [x] (= 3 x)) (vals (frequencies (mapv rank hand))))) true))

(defn four-of-a-kind? [hand]
 (contains? (set (mapv (fn [x] (= 4 x)) (vals (frequencies (mapv rank hand))))) true))

(defn flush? [hand]
 (apply = (mapv suit hand)))

(defn full-house? [hand]
 (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
(or (= (count (filterv (fn [x] (= 2 x))(vals (frequencies (mapv rank hand))))) 2) (four-of-a-kind? hand)))

(defn straight? [hand]
(let [a  (sort (mapv rank hand))
      b (apply min a)]  (or (= a (range b (+ 5 b))) (= a (sort(conj (range 2 6) 14))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)


(defn value [hand]
 (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
   (apply max(mapv (fn [x] (if((first x) hand) (second x) 0)) checkers) )))
