(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (let [rank-list (fn [card] (rank card))]
    (map rank-list hand)))

(defn suits [hand]
  (let [suit-list (fn [card] (suit card))]
    (map suit-list hand)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (ranks hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (ranks hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (ranks hand)))) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (suits hand)))) 5))


(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (== (apply min (vals (frequencies (ranks hand)))) 2)))

(defn two-pairs? [hand]
  (let [rankies (sort (vals (frequencies (ranks hand))))]
    (or (= rankies (seq [1 2 2]))
        (= rankies (seq [1 4])))))



(defn straight? [hand]
  (let [rankies-ace-ace (sort (ranks hand))
        rankies-ace-one (sort (replace {14 1} (ranks hand)))
        min-rank (fn [rankies] (apply min rankies))
        max-rank (fn [rankies] (+ (min-rank rankies) 5))
        straight (fn [rankies] (range
                                 (min-rank rankies)
                                 (max-rank rankies)))
        straight-or-not (fn [rankies] (= (straight rankies) rankies))]
    (or (straight-or-not rankies-ace-ace)
        (straight-or-not rankies-ace-one))))


(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands (filter (fn [x] ((first x) hand)) checkers)
        values (map second hands)]
    (apply max values)))



