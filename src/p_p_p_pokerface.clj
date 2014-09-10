(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        higher-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get higher-ranks r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= 1 (count suits))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (frequencies (map rank hand))
        pairs (filter (fn [x] (= 2 x)) (vals freqs))]
    (= 2 (count pairs))))

(defn straight? [hand]
  (let [s1 (sort (map rank hand))
        s1-min (apply min s1)
        s2 (sort (replace {14 1} (map rank hand)))
        s2-min (apply min s2)]
    (or
     (= s1 (range s1-min (+ s1-min 5)))
     (= s2 (range s2-min (+ s2-min 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-this-out
        (fn [checker] (if ((first checker) hand) (second checker)))]
    (apply max (filter identity (map check-this-out checkers)))))
