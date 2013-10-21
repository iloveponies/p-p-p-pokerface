(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        mappings {\T 10, \J 11, \Q 12, \K 13, \A 14}]
     (if(Character/isDigit r)
       (Integer/valueOf (str r))
       (Integer/valueOf (str (mappings r))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-freq-values [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (= 4 (count (rank-freq-values hand))))

(defn has-amount-of-same? [values n]
  (not (empty? (filter (fn [value] (= value n)) values))))

(defn three-of-a-kind? [hand]
  (has-amount-of-same? (rank-freq-values hand) 3))

(defn four-of-a-kind? [hand]
  (has-amount-of-same? (rank-freq-values hand) 4))

(defn flush? [hand]
  (let [suit-values (vals (frequencies (map suit hand)))]
    (= 1 (count suit-values))))

(defn full-house? [hand]
  (= [2 3] (sort (rank-freq-values hand))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (rank-freq-values hand))) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (map rank hand))
        sorted-ranks-ace-first (sort (replace {14 1} ranks))]
    (or (= sorted-ranks (range (apply min sorted-ranks) (+ 1 (apply max sorted-ranks))))
        (= sorted-ranks-ace-first (range 1 6)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn why-on-earth-wouldnt-i-use-cond-to-determine-the-value-of-this [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0)
  )

(defn value [hand]
  (why-on-earth-wouldnt-i-use-cond-to-determine-the-value-of-this hand))
