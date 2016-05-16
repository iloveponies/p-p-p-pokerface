(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ x] card]
    (str x)))

(def card-values
  { \T 10
    \J 11
    \Q 12
    \K 13
    \A 14 })

(defn rank [card]
  (let [[x _] card]
    (if (Character/isDigit x)
        (Integer/valueOf (str x))
        (get card-values x))))

(defn hasRank? [hand y]
  (let [ranks (map rank hand)
        frq (vals (frequencies ranks))]
  (< 0 (count (filter (fn [x] (= x y)) frq)))))

;---

(defn pair? [hand]
  (hasRank? hand 2))

(defn three-of-a-kind? [hand]
  (hasRank? hand 3))

(defn four-of-a-kind? [hand]
  (hasRank? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suitfrq (vals (frequencies suits))]
  (= 5 (apply min suitfrq))))

(defn sorted-ranks [hand]
  (let [ranks (map rank hand)]
   (sort (vals (frequencies ranks)))))

(defn full-house? [hand]
  (let [rankfq (sorted-ranks hand)]
    (= [2  3] rankfq)))

(defn two-pairs? [hand]
  (let [ranks (sorted-ranks hand)]
    (or
    (= [1 2 2] ranks)
    (= [1 4] ranks))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low  (sort (replace {14 1} ace-high))
        straight?  (fn [seq]
                     (= (range (apply min seq) (+ 5 (apply min seq))) seq))]
  (or (straight? ace-high) (straight? ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

;---

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checks #{[high-card? 0][pair? 1][two-pairs? 2][three-of-a-kind? 3][straight? 4][flush? 5][full-house? 6][four-of-a-kind? 7][straight-flush? 8]}
        check-value (fn [c]
                      (if ((first c) hand)
                      (second c) false))
        all-values (map check-value checks)
        not-false (filter (fn [x] x) all-values)]
  (apply max not-false)))
