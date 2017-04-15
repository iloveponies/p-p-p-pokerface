(ns p-p-p-pokerface)

(def replacements
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and
      (contains? (set ranks) 2)
      (not (== (count (set ranks)) (count ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (and
      (contains? (set ranks) 3))))

(defn four-of-a-kind? [hand]
  (let [ranks (vals (frequencies (map rank hand)))]
    (contains? (set ranks) 4)))

(defn flush? [hand]
  (let [ranks (sort (map rank hand))
        suits (set (map suit hand))]
    (and (apply < ranks)
         (== 1 (count suits)))))

(defn full-house? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (and (== 2 (first freqs))
         (== 3 (second freqs)))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (let [freqs (sort (vals (frequencies (map rank hand))))
            twos (filter (fn [x] (== x 2)) freqs)
            num-twos (count twos)]
        (== 2 num-twos))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        max-rank (apply max ranks)
        min-rank (apply min ranks)]
    (if (and (== min-rank 2)
             (== max-rank 14))
      (let [real-min-rank 1
            real-max-rank (nth ranks 3)]
        (= (sort (replace {14 1} ranks))
           (range real-min-rank (+ real-max-rank 1))))
      (= ranks (range min-rank (+ max-rank 1))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-type? [hand checker-value]
  ((first checker-value) hand))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [x] (let [[_ y] x] (if (hand-has-type? hand x) y 0))) checkers))))
