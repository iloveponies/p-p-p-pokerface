(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (let [{rank 0} card]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get values rank)))))

(defn suit [card]
  (let [{suit 1} card]
    (str suit)))

(defn rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (let [rank-frequencies (vals (frequencies ranks))]
      rank-frequencies)))

(defn multiple? [hand number]
  (let [conts (fn [x] (== x number))]
    (not (empty? (filter conts (rank-frequencies hand))))))

(defn pair? [hand]
  (multiple? hand 2))

(defn three-of-a-kind? [hand]
  (multiple? hand 3))

(defn four-of-a-kind? [hand]
  (multiple? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (let [[x] (max (vals (frequencies suits)))]
      (== 5 x))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [conts (fn [x] (== x 2))]
    (let [two (get (frequencies (rank-frequencies hand)) 2)]
      (let [four (get (frequencies (rank-frequencies hand)) 4)]
        (cond
          two (== 4 (* two 2))
          four (== 4 (* four 4))
          :else false)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (let [maximum (apply max ranks)]
      (let [minimum (apply min ranks)]
        (let [straight (range minimum (+ maximum 1))]
        (if (and (= minimum 2) (= maximum 14)) 
         (let [banks (sort (replace {14 1} ranks))]
           (if (= banks (range 1 6))
              true
              false))
         (let [banks ranks]
           (if (= banks straight)
              true
              false))))))))

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
