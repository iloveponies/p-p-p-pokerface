(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10
                \J 11
                \Q 12
                \K 13
                \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (values r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (not (nil? (some #{2} (vals (frequencies ranks)))))))


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (not (nil? (some #{3} (vals (frequencies ranks)))))))


(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (not (nil? (some #{4} (vals (frequencies ranks)))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        value-freq (vals (frequencies ranks))
        pair-count (count (filter (fn [f] (= 2 f)) value-freq))]
    (or
      (= 2 pair-count)
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [straight-from-min (fn [lower-card]
                            (range lower-card (+ 5 lower-card)))
        sorted-hand-ace (sort (map rank hand))
        min-hand-ace (apply min sorted-hand-ace)
        straight-ace (straight-from-min min-hand-ace)
        sorted-hand-one (sort (replace {14 1} sorted-hand-ace))
        min-hand-one (apply min sorted-hand-one)
        straight-one (straight-from-min min-hand-one)]
    (or (= sorted-hand-ace straight-ace) (= sorted-hand-one straight-one))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
                   [straight-flush? 8]}]
    (apply max (map second
         (filter (fn [checker]
              ((first checker) hand)) checkers)))))

