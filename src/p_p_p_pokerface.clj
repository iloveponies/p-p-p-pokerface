(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
    (let [[rank _] card
        digit? (fn [x] (Character/isDigit x))]
    (if (digit? rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suite] card]
    (str suite)))

;Checks if all the cards are distinct
(defn all-cards-distinct? [hand] (= (count (map rank hand)) (count (set (map rank hand)))))

;Returns the values of the frequencies i.e. the frequency
(defn vals-frequencies-rank [hand]
  (vals (frequencies (map rank hand))))

;Returns the count of ranks post filter
(defn count-cards-post-filter [hand filter-fn pair-count]
  (= (count (seq (filter filter-fn (vals-frequencies-rank hand)))) pair-count))

;Returns ranks of the hand in the sorted manner
(defn sorted-ranks-for-hand [hand] (vec (sort (map rank hand))))

;Checks if the suit is the same in a hand
(defn same-suit-hand? [hand] (= (count (set (map suit hand))) 1))

(defn pair? [hand]
   (let [get-all-ranks (fn [x] (map rank x))
       ranks (get-all-ranks hand)
       pair-no 1
       filterFrequency 2
       all-cards-distinct? (fn [] (= (count ranks) (count (set ranks))))]
   (if (not (all-cards-distinct?))
            (= (count (seq (filter (fn [x] (= x filterFrequency)) (vals (frequencies ranks))))) pair-no) false
     )))

(defn three-of-a-kind? [hand]
    (let [ filter-three-of-a-kind (fn [x] (= x 3))
         filter-one-of-a-kind (fn [x] (if (= x 1) x))
          count-of-three-pair 1
           count-of-no-pair 2
         three-of-kind? (cond
                         (and (count-cards-post-filter hand filter-three-of-a-kind count-of-three-pair)
                              (count-cards-post-filter hand filter-one-of-a-kind count-of-no-pair)) true
                         :else false)]
    (if (not (all-cards-distinct? hand))
    three-of-kind? false)))

(defn four-of-a-kind? [hand]
      (let [ filter-four-of-a-kind (fn [x] (= x 4))
         filter-one-of-a-kind (fn [x] (= x 1))
          count-of-four-pair 1
           count-of-no-pair 1
         four-of-kind? (cond
                         (and (count-cards-post-filter hand filter-four-of-a-kind count-of-four-pair)
                              (count-cards-post-filter hand filter-one-of-a-kind count-of-no-pair)) true
                         :else false)]

    (if (not (all-cards-distinct? hand))
    four-of-kind? false)))

(defn flush? [hand]
  (let [sorted-hand (sorted-ranks-for-hand hand)
         first-el (get sorted-hand 0)
        last-el (get sorted-hand 4)
        same-suit-hand? (same-suit-hand? hand)]
    (cond (and same-suit-hand? (not (= (range first-el (+ last-el 1)) sorted-hand)) ) true
          :else false
          )
    ))

(defn full-house? [hand]
  (let [ filter-three-of-a-kind (fn [x] (= x 3))
         filter-two-of-a-kind (fn [x] (= x 2))
          count-of-three-of-a-kind 1
           count-of-pair 1
         full-house-predicate? (cond
                         (and (count-cards-post-filter hand filter-three-of-a-kind count-of-three-of-a-kind)
                              (count-cards-post-filter hand filter-two-of-a-kind count-of-pair)) true
                         :else false)]

    (if (not (all-cards-distinct? hand))
    full-house-predicate? false)))

(defn two-pairs? [hand]
     (let [get-all-ranks (fn [x] (map rank x))
       ranks (get-all-ranks hand)
       pair-no 2
       filterFrequency 2
       all-cards-distinct? (fn [] (= (count ranks) (count (set ranks))))]
   (if (not (all-cards-distinct?))
            (or (= (count (seq (filter (fn [x] (= x filterFrequency)) (vals (frequencies ranks))))) pair-no) (four-of-a-kind? hand))
     false
     )))

(defn straight? [hand]
    (let [sorted-hand-1 (sorted-ranks-for-hand hand)
         first-el-1 (get sorted-hand-1 0)
        last-el-1 (get sorted-hand-1 4)
          sorted-hand-2 (sort (replace {14 1} (map rank hand)))
          first-el-2 (apply min sorted-hand-2)
          last-el-2 (apply max sorted-hand-2)
        same-suit-hand? (same-suit-hand? hand)]
    (cond (and (not same-suit-hand?)
               (or (= (range first-el-1 (+ last-el-1 1)) sorted-hand-1)
                   (= (range first-el-2 (+ last-el-2 1)) sorted-hand-2))) true
          :else false
          )
    ))

(defn straight-flush? [hand]
   (let [sorted-hand-1 (sorted-ranks-for-hand hand)
         first-el-1 (get sorted-hand-1 0)
        last-el-1 (get sorted-hand-1 4)
          sorted-hand-2 (sort (replace {14 1} (map rank hand)))
          first-el-2 (apply min sorted-hand-2)
          last-el-2 (apply max sorted-hand-2)
        same-suit-hand? (same-suit-hand? hand)]
    (cond (and same-suit-hand? (or (= (range first-el-1 (+ last-el-1 1)) sorted-hand-1)
                                   (= (range first-el-2 (+ last-el-2 1)) sorted-hand-2)) ) true
          :else false
          )
    ))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))

    )
  )


