(ns p-p-p-pokerface)

(def replacement {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card
        rank-value (fn [rank] (if (Character/isDigit rank) (Integer/valueOf (str rank))))
        rank (rank-value fst)]
    (if (nil? rank) (replacement fst) rank)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-kind [n hand]
  (if (some #(= n %) (vals (frequencies (map #(rank %) hand)))) true false))

(defn pair? [hand]
  (n-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-kind 4 hand))

(defn flush? [hand]
  (let [suits (map #(suit %) hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map #(rank %) hand)))]
    (cond
     (some #(= 4 %) freq) true
     (some #(= 0 (mod % 2)) (vals (frequencies (filter #(= 2 %) freq)))) true
      :else false)))

(defn sorted-ranks [hand]
  (sort (map #(rank %) hand)))

(defn replace-rank [hand]
  (let [ranks (map #(rank %) hand)]
    (if (some #(= 2 %) ranks) (replace {14 1} ranks) ranks)))

(defn straight? [hand]
  (let [sort-ranks (sort (replace-rank hand))
        fst (first sort-ranks)
        straight (range fst (+ fst 5))]
    (= sort-ranks straight)))

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
    (apply max (map #(if ((first %) hand) (second %) 0) checkers))))


