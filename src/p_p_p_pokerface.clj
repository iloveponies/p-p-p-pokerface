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
   (filter (fn [asdf] (= asdf amount))
           (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (>= (amount-of-certain-amount-of-same-ranks hand 2) 1))

(defn four-of-a-kind? [hand]
  (= (amount-of-certain-amount-of-same-ranks hand 4) 1))

(defn two-pairs? [hand]
  (or (= (amount-of-certain-amount-of-same-ranks hand 2) 2)
      (four-of-a-kind? hand)))

(defn three-of-a-kind? [hand]
  (= (amount-of-certain-amount-of-same-ranks hand 3) 1))

(defn straight? [hand]
  ;TODO
  )

(defn straight? [hand]
  )

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (= (amount-of-certain-amount-of-same-ranks hand 2) 1)
           (three-of-a-kind? hand)
           (= (amount-of-certain-amount-of-same-ranks hand 1) 0)))

(defn straight-flush? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)