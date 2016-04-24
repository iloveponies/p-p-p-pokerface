(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (str (let [[_ s] card] s)))

(defn n-of-a-kind? [hand count]
  (contains? (set (vals (frequencies (map rank hand)))) count))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn same-suit? [hand]
  (let [s1 (suit (first hand))]
    (every? (fn [c] (= s1 (suit c))) (rest hand))))

(defn in-sequence? [hand]
  (let [ranks (sort < (map rank hand))
        diffs (sort > (map - (rest ranks) ranks))]
    (= '(1 1 1 1) diffs)))

(defn flush? [hand]
  (and (same-suit? hand) (not (in-sequence? hand))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (= '(2 2 1) (sort > (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (and (not (same-suit? hand))
       (or (in-sequence? hand)
           (= '(2 3 4 5 14) (sort < (map rank hand)))))) ;; <- low ace straight

(defn straight-flush? [hand]
  (and same-suit? hand)
       (or (in-sequence? hand)
           (= '(2 3 4 5 14) (sort < (map rank hand))))) ;; <- low ace straight

(defn value [hand]
  (cond (straight-flush?  hand) 8
        (four-of-a-kind?  hand) 7
        (full-house?      hand) 6
        (flush?           hand) 5
        (straight?        hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs?       hand) 2
        (pair?            hand) 1
        :else                   0))

