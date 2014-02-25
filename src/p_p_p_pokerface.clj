(ns p-p-p-pokerface)

(defn rank [card]
  (let [letter-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (letter-ranks r))))

(defn suit [card]
  (let [[_ s] card]
      (str s)))

(defn hand-count [extract hand]
  (-> extract (map hand) frequencies vals))

(defn max-hand-count [extract hand]
  (apply max (hand-count extract hand)))

(defn pair? [hand]
    (> (max-hand-count rank hand) 1))

(defn three-of-a-kind? [hand]
  (> (max-hand-count rank hand) 2))

(defn four-of-a-kind? [hand]
  (> (max-hand-count rank hand) 3))

(defn flush? [hand]
  (== (max-hand-count suit hand) 5))

(defn full-house? [hand]
  (= (sort (hand-count rank hand)) [2 3]))

(defn two-pairs? [hand]
  (= (sort (hand-count rank hand)) [1 2 2]))

(defn straight? [hand]
  (let [ranks1 (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks1))
        sequential? (fn [xs] (= xs (range (apply min xs) (+ (apply max xs) 1))))]
    (or (sequential? ranks1) (sequential? ranks2))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
