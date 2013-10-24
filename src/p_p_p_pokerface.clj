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
  (let [sorted-ranks (sort (map rank hand))
        no-friequencies? (== 5 (count (set sorted-ranks)))]
    (cond
      (and (no-friequencies?) (== 4 (- (max sorted-ranks) (min sorted-ranks)))) true
      (and (no-friequencies?) (and (== 14 (max sorted-ranks)) (== 2 (min sorted-ranks)))
           (== 4 (- (max (sort (replace {14 1} sorted-ranks))) (min (sort (replace {14 1} sorted-ranks)))))) true
     :else false)))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
