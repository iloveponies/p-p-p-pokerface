(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        map {\T 10, \J 11, \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get map r))))

(defn at-least [n hand]
  (let [ranks (map rank hand)]
    (<= n (apply max (vals (frequencies ranks))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (at-least 2 hand))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        ge (fn [x] (>= x 2))
        filtered (filter ge (vals (frequencies ranks)))]
    (and (not (empty? filtered)) 
         (or (== (count filtered) 2) (== 4 (first filtered))))))

(defn three-of-a-kind? [hand]
  (at-least 3 hand))

(defn four-of-a-kind? [hand]
  (at-least 4 hand))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        rep1 (sort (replace {1 14} ranks))
        rep2 (sort (replace {14 1} ranks))]
    (or (= ranks (range (first ranks) (+ (first ranks) 5)))
             (= rep1 (range (first rep1) (+ (first rep1) 5)))
             (= rep2 (range (first rep2) (+ (first rep2) 5))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 1 (count (set suits)))))

(defn full-house? [hand]
  (let [kinds (vals (frequencies (map rank hand)))]
    (== 2 (count kinds))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers [high-card? pair? two-pairs? three-of-a-kind? straight? 
                   flush? full-house? four-of-a-kind? straight-flush?]
        hand-has-value? (fn [hand val] ((get checkers val) hand))
        predicate (fn [x] (hand-has-value? hand x))]
        (apply max (filter predicate (range 9)))))