(ns p-p-p-pokerface)

(def high-rank {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (high-rank rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map #(rank %) hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map #(rank %) hand)]
    (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map #(rank %) hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map #(suit %) hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [ranks (map #(rank %) hand)]
    (or
      (= (sort (vals (frequencies ranks))) [1 2 2])
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map #(rank %) hand)
        new-ranks (if (contains? (set ranks) 13) ranks (replace {14 1} ranks))
        min-rank (apply min new-ranks)
        max-rank (apply max new-ranks)
        comp-hand (range min-rank (inc max-rank))]
    (= (sort new-ranks) comp-hand)))

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
