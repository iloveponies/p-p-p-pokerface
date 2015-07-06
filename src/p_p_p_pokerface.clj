(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first-character _] card]
    (if (Character/isDigit first-character)
      (Integer/valueOf (str first-character))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} first-character))))

(defn suit [card]
  (str (second (seq card))))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [x (sort (vals (frequencies (map rank hand))))]
    (or (= x [1 2 2]) (= x [1 4])))) 

(defn straight? [hand]
  (let [regular-rank (sort (map rank hand))
        ace-low-rank (sort (replace {14, 1} (map rank hand)))
        hand-range (fn [modified-hand] 
                     (range (apply min modified-hand) (inc (apply max modified-hand))))]
    (or (= regular-rank (hand-range regular-rank))
        (= ace-low-rank (hand-range ace-low-rank)))))

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


