(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        chars-rank {\T 10, \J 11, \Q 12, \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (chars-rank fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (< 1 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 2 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 3 (apply max (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (< 4 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    ( = '(2 3) (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (< 1 (count (filter (fn [r] (< 1 r))
                        (vals (frequencies ranks)))))))

(defn straight? [hand]
  (let [straight-ranks? (fn [ranks]
                          (= (sort ranks)
                             (range (apply min ranks) (+ (apply min ranks) 5))))
        ranks1 (map rank hand)
        ranks2 (replace {14 1} ranks1)]
    (or (straight-ranks? ranks1)
        (straight-ranks? ranks2))))

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
    (apply max (map (fn [[check v]] (if (check hand) v 0)) checkers))))
