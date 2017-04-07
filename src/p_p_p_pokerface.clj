(ns p-p-p-pokerface)

(def face-card-rank {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rnk sut] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get face-card-rank rnk))))

(defn suit [card]
  (let [[rnk sut] card]
    (str sut)))

(defn check-number-of-hands? [hand n]
  (let [hand-ranks (map rank hand)
        rank-freq (set
                      (vals (frequencies hand-ranks)))]
    (contains? rank-freq n)))

(defn pair? [hand]
  (check-number-of-hands? hand 2))

(defn three-of-a-kind? [hand]
  (check-number-of-hands? hand 3))

(defn four-of-a-kind? [hand]
  (check-number-of-hands? hand 4))

(defn flush? [hand]
  (let [hand-suits (map suit hand)
        suit-frequencies (set (vals (frequencies hand-suits)))]
    (contains? suit-frequencies 5)))

(defn full-house? [hand]
  (and (check-number-of-hands? hand 3) (check-number-of-hands? hand 2)))

(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)
        rank-frequencies
          (frequencies
            (apply vector
              (vals (frequencies hand-ranks))))]
    (= (get rank-frequencies 2) 2)))

(defn straight? [hand]
  (let [hand-ranks (apply vector (sort (map rank hand)))
        hand-ranks-ace (apply vector (sort (replace {14 1} (map rank hand))))
        hand-ranks-first (first hand-ranks)
        hand-ranks-ace-first (first hand-ranks-ace)]
    (or (= (range hand-ranks-first (+ hand-ranks-first 5)) hand-ranks)
        (= (range hand-ranks-ace-first (+ hand-ranks-ace-first 5)) hand-ranks-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [hand-value-map #{[high-card? 0]  [pair? 1]
                         [two-pairs? 2]  [three-of-a-kind? 3]
                         [straight? 4]   [flush? 5]
                         [full-house? 6] [four-of-a-kind? 7]
                         [straight-flush? 8]}
        applicable-values (filter (fn [x] ((first x) hand)) hand-value-map)]
   println (apply max (map second applicable-values))))
