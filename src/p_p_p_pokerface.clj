(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= [5] (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
;  (let [freq-vals (vals (frequencies (map rank hand)))]
;    (or (= freq-vals [2 3])
;        (= freq-vals [3 2]))))
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (>= 1 (get (frequencies (vals (frequencies (map rank hand)))) 1)))

(defn straight? [hand]
  (let [ranked-hand (sort (map rank hand))
        de-aced-hand (sort (replace {14 1} ranked-hand))
        straighty? (fn [x] (and (apply < x)
                                (== 4 (- (last x)(first x)))))]
    (or (straighty? ranked-hand)
        (straighty? de-aced-hand))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
