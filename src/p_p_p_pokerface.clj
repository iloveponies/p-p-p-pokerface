(ns p-p-p-pokerface)

(def replacements
  {\T 10,
  \J 11,
  \Q 12
  \K 13,
  \A 14})

(defn rank [[rank _]]
  (cond
   (Character/isDigit rank) (Integer/valueOf (str rank))
   :else (replacements rank)))

(defn suit [[_ suit]]
  (str suit))

(defn number-of? [what, number, hand]
  (>= (apply max (vals (frequencies (map what hand)))) number))

(defn pair? [hand]
  (number-of? rank 2 hand))

(defn three-of-a-kind? [hand]
  (number-of? rank 3 hand))

(defn four-of-a-kind? [hand]
  (number-of? rank 4 hand))

(defn flush? [hand]
  (number-of? suit 4 hand))

(defn full-house? [hand]
  (let [ranks (sort (map rank hand)),
        freq (set (vals (frequencies ranks)))]

    (and (= (count freq) 2)
         (and (contains? freq 2)
              (contains? freq 3)))))

(defn two-pairs? [hand]
  (let [ranks (sort (map rank hand)),
        freq (vals (frequencies ranks))
        number-of-elements-eq?
        (fn [expected-element, frequency]
          (= (count (filter (fn [x] (= x expected-element)) freq)) frequency))]
    (or (number-of-elements-eq? 2 2)
        (number-of-elements-eq? 4 1 ))))

(defn straight? [hand]
  (let [ranks (map rank hand),
        ranks (if (empty? (filter (fn [item] (<= item 5)) ranks ))
               ranks
               (replace {14 1} ranks))
        ranks (sort ranks)
        ranks-range (range (first ranks) (+ (last ranks) 1))]
    (= ranks ranks-range)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
      (apply max (map second (filter (fn [check] ((first check) hand)) checkers)))
    ))

