(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str ({\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn number-of-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn number-of-suit[hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn pair? [hand]
  (< 1 (number-of-rank hand)))

(defn three-of-a-kind? [hand]
  (< 2 (number-of-rank hand)))

(defn four-of-a-kind? [hand]
  (< 3 (number-of-rank hand)))

(defn flush? [hand]
  (= 5 (number-of-suit hand)))

(defn pairs? [amount hand]
  (= amount (count(filter #(= 2 %)(vals (frequencies (map rank hand)))))))

(defn full-house? [hand]
  (and (pairs? 1 hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (pairs? 2 hand))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))]
    (cond
      (= (last sorted-ranks) 14) (or (= (conj (butlast sorted-ranks) 1) (range 1 (+ 1 (last (butlast sorted-ranks)))))
                                     (= sorted-ranks (range (first sorted-ranks) (+ 1 (last sorted-ranks)))))
      :else (= sorted-ranks (range (first sorted-ranks) (+ 1 (last sorted-ranks)))))))

(defn straight-flush? [hand]
  (and (= 1 (count (distinct (map suit hand))))
       (straight? hand)))

(defn value [hand]
  nil)
