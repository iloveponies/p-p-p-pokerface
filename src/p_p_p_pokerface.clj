(ns p-p-p-pokerface)

(defn card-value [rank]
  (let [final (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank)]
    (if (= final nil) rank final)))

(defn rank [swell]
  (let [[wanna-be --] swell]
    (Integer/parseInt(str (card-value wanna-be)))))

(defn suit [swell]
  (let [[-- wanna-be] swell]
  (str wanna-be)))

(defn amount-of-certain-amount-of-same-ranks [hand amount]
  (count
   (filter (fn [freq-vals]
             (= freq-vals amount))
    (vals (frequencies
           (map rank hand))))))

(defn pair? [hand]
  (>= (amount-of-certain-amount-of-same-ranks hand 2) 1))

(defn three-of-a-kind? [hand]
  (= (amount-of-certain-amount-of-same-ranks hand 3) 1))

(defn four-of-a-kind? [hand]
  (= (amount-of-certain-amount-of-same-ranks hand 4) 1))

(defn two-pairs? [hand]
  (or (= (amount-of-certain-amount-of-same-ranks hand 2) 2)
      (four-of-a-kind? hand)))

(defn two-map [seqv]
  (map vector seqv (rest seqv)))

(defn straight? [hand]
  (let [ranked-hand (map rank hand)
        diff=1? (fn [[one two]]
                     (=(- two one) 1))
        func (fn [handy]
                 (contains? (set (map diff=1? (two-map (sort handy)))) false))]
    (if (func ranked-hand)
      (if (contains? (set ranked-hand) 14)
        (not (func (replace {14 1} ranked-hand)))
        false)
      true)))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (= (amount-of-certain-amount-of-same-ranks hand 2) 1)
           (three-of-a-kind? hand)
           (= (amount-of-certain-amount-of-same-ranks hand 1) 0)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(def check-this-shit 
  {pair? 1
   two-pairs? 2
   three-of-a-kind? 3
   straight? 4
   flush? 5
   full-house? 6
   four-of-a-kind? 7
   straight-flush? 8}
  )

(defn value [hand]
  (apply max
         (map
          (fn [[func value]]
            (if (func hand) value 0))
          check-this-shit)))