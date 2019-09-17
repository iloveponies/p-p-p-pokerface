(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      ({\T 10,
        \J 11,
        \Q 12
        \K 13
        \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (frequencies ranks)))

(defn- n-of-a-kind [n hand]
  (let [rank-freq (rank-frequencies hand)]
    (filter #(= % n) (vals rank-freq))))

(defn- n-of-a-kind? [n hand]
  (let [of-kind (n-of-a-kind n hand)]
    (not (empty? of-kind))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= (count suits) 1)))

(defn full-house? [hand]
  (let [rank-freq (rank-frequencies hand)]
    (= (set (vals rank-freq)) #{2 3})))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (count (n-of-a-kind 2 hand))
         2)))

(defn straight? [hand]
  (let [aces-high-ranks (sort (map rank hand))
        aces-low-ranks (sort (replace {14 1} aces-high-ranks))
        min-rank-aces-high (apply min aces-high-ranks)
        min-rank-aces-low (apply min aces-low-ranks)]
    (or (= (range min-rank-aces-high (+ min-rank-aces-high 5))
           aces-high-ranks)
        (= (range min-rank-aces-low (+ min-rank-aces-low 5))
           aces-low-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn- high-card? [hand]
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
        values (map #(if ((first %) hand) (second %) -1) checkers)]
    (apply max values)))
