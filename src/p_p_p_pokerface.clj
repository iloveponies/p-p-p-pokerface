(ns p-p-p-pokerface)


(defn rank [card]
  (let [[fst snd] card
        values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (values fst) )
    ))

(defn suit [card]
  (let [[_, s] card]
    (str  s)))

(defn most-frequent [f hand]
  (let [ranks (frequencies (map f hand))
        maxnum (apply max (vals ranks))]
    maxnum)
  )

(defn pair? [hand]
  (< 1 (most-frequent rank hand)))

(defn three-of-a-kind? [hand]
  (< 2 (most-frequent rank hand))
  )

(defn four-of-a-kind? [hand]
  (< 3 (most-frequent rank hand)))

(defn flush? [hand]
  (= 5 (most-frequent suit hand)))

(defn full-house? [hand]
  (->> hand
       (map rank)
       frequencies
       vals
       sort
       (= (sort  (list 2 3)))))

(defn two-pairs? [hand]
  (->> hand
       (map rank)
       frequencies
       vals
       sort
       (= (sort (list 2 2 1)))))

(defn straight? [hand]
  (let [sorted-rank (->> hand
                         (map rank)
                         (filter #(not (= 14 %)))
                         sort)
        m (apply min sorted-rank)]
    (if (= 5 (count sorted-rank))
      (= sorted-rank (range m (+ m 5)))
      (= sorted-rank (range m (+ m 4)))
      )))

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
