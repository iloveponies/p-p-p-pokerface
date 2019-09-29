(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        num-char (if (Character/isDigit r) r (get replacements r))]
    (Integer/valueOf (str num-char))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        max-count (apply max counts)]
    (> max-count 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        max-count (apply max counts)]
    (> max-count 2)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        max-count (apply max counts)]
    (> max-count 3)))

(defn flush? [hand]
  (let [suits (map suit hand)
        counts (vals (frequencies suits))
        max-count (apply max counts)]
    (= max-count 5)))


(defn full-house? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        sorted-counts (sort counts)]
    (= [2 3] sorted-counts)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        sorted-counts (sort counts)]
    (or (= [1 2 2] sorted-counts)
        (= [1 4] sorted-counts))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ranks (replace {14 1} ranks)
        reg-min (apply min ranks)
        low-min (apply min low-ranks)
        rank-sub (map #(- % reg-min) (sort ranks))
        low-rank-sub (map #(- % low-min) (sort low-ranks))]
    (or (= (range 5) rank-sub)
        (= (range 5) low-rank-sub))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (->> checkers
         (filter #((first %) hand))
         (map second)
         (apply max))))
