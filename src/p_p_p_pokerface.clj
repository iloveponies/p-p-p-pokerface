(ns p-p-p-pokerface)

(defn rank [card]
  (let [[first _] card
        suit-mapping {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (suit-mapping first))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-counts (frequencies ranks)]
    (== 2 (apply max (vals rank-counts)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-counts (frequencies ranks)]
    (== 3 (apply max (vals rank-counts)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-counts (frequencies ranks)]
    (== 4 (apply max (vals rank-counts)))))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-set (set suits)]
    (== 1 (count suit-set))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
       rank-counts (frequencies ranks)]
    (= [2 3] (sort (vals rank-counts)))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-counts (frequencies ranks)]
    (or (= [1 2 2] (sort (vals rank-counts)))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace-ranks (replace {14 1} ranks)
        min-rank (apply min ranks)
        min-low-ace-rank  (apply min low-ace-ranks)
        ranks-sub-min (map (fn [x] (- x min-rank)) ranks)
        low-ace-ranks-sub-min (map (fn [x] (- x min-low-ace-rank)) low-ace-ranks)]
    (or (= (range 0 5) (sort ranks-sub-min))
        (= (range 0 5) (sort low-ace-ranks-sub-min)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ;All hands have a high card

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        valid (filter (fn [x] ((first x) hand)) checkers)
        values (map second valid)]
    (apply max values)))

