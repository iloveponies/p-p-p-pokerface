(ns p-p-p-pokerface)

(def replacements {\T 10,
                   \J 11,
                   \Q 12,
                   \K 13,
                   \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (replacements fst))))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
 (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
 (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
 (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
 (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (<= 4 (apply + (filter
                    (fn [x] (< 1 x))
                    (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [valz (sort (map rank hand))]
   (cond
    (and (apply < valz) (== (+ 4 (apply min valz)) (apply max valz))) true
    (= valz (seq [2 3 4 5 14])) true
    :else false)))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :else 0))
