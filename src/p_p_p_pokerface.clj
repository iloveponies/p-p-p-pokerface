(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      ({\T 10
        \J 11
        \Q 12
        \K 13
        \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn combos [hand func]
  (frequencies (map func hand)))

(defn same-kind? [hand func condition]
  (>= (apply max (vals (combos hand func))) condition))

(defn pair? [hand]
  (same-kind? hand rank 2))

(defn three-of-a-kind? [hand]
  (same-kind? hand rank 3))

(defn four-of-a-kind? [hand]
  (same-kind? hand rank 4))

(defn flush? [hand]
  (same-kind? hand suit 5))

(defn full-house? [hand]
  (let [counts (vals (combos hand rank))]
    (and
     (= (apply min counts) 2)
     (= (apply max counts) 3))))

(defn two-pairs? [hand]
  (let [counts (vals (combos hand rank))]
    (or
     (= (get (frequencies counts) 2) 2)
     (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ordered-a (sort ranks)
        ordered-b (sort (replace {14 1} ranks))
        check (fn [ordered-ranks]
                (and
                 (apply < ordered-ranks)
                 (= (apply min ordered-ranks) (- (apply max ordered-ranks) 4))))]
    (or (check ordered-a) (check ordered-b))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        match (fn [checker] ((first checker) hand))]
   (apply max (map second (filter match checkers)))))
