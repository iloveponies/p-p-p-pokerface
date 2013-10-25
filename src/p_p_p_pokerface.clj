(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [occurance (set (vals(frequencies (map rank hand))))]
     (contains? occurance 2)))

(defn three-of-a-kind? [hand]
   (let [occurance (set (vals(frequencies (map rank hand))))]
    (contains? occurance 3)))

(defn four-of-a-kind? [hand]
   (let [occurance (set (vals(frequencies (map rank hand))))]
    (contains? occurance 4)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (let [occurance (vals (frequencies (map rank hand)))]
     (and (== 3 (count occurance))
          (pair? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (if (and (== 14 (apply max ranks)) (== 2 (apply min ranks)))
    (sort (replace {14 1} (vec ranks)))
    (sort ranks))]
  (= sorted-ranks
      (range (apply min sorted-ranks) (inc (apply max sorted-ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        hand-has-value? (fn [hand i] ((get checkers i) hand))]
    (cond
     (hand-has-value? hand 8) 8
     (hand-has-value? hand 7) 7
     (hand-has-value? hand 6) 6
     (hand-has-value? hand 5) 5
     (hand-has-value? hand 4) 4
     (hand-has-value? hand 3) 3
     (hand-has-value? hand 2) 2
     (hand-has-value? hand 1) 1
     :else 0)))
