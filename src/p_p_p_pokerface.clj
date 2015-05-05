(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (replacements rank))))

(defn suit [card]
 (let [[_ suit] card]
   (str suit)))

(defn pair? [hand]
  (let [foo (fn [x] (< 1 x))]
   (contains? (set (map foo (vals (frequencies (map rank hand))))) true)))

(defn three-of-a-kind? [hand]
  (let [foo (fn [x] (< 2 x))]
   (contains? (set (map foo (vals (frequencies (map rank hand))))) true)))

(defn four-of-a-kind? [hand]
  (let [foo (fn [x] (< 3 x))]
  (contains? (set (map foo (vals (frequencies (map rank hand))))) true)))

(defn flush? [hand]
 (let [foo (fn [x] (< 4 x))]
  (contains? (set (map foo (vals (frequencies (map suit hand))))) true)))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [r1 (sort (set(map rank hand)))
        r2 (sort (replace {14 1} r1))]
    (or (= r1 (range (first r1) (+ (first r1) 5)))
        (= r2 (range (first r2) (+ (first r2) 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))
