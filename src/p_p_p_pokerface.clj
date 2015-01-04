(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rpl {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rpl fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))]
    (contains? (set counts) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))]
    (contains? (set counts) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))]
    (contains? (set counts) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        counts (vals (frequencies suits))]
    (contains? (set counts) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))]
    (and (contains? (set counts) 2)
         (contains? (set counts) 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        counts (vals (frequencies ranks))
        counts-of-counts (frequencies counts)]
    (or (= 2 (get counts-of-counts 2))
        (= 1 (get counts-of-counts 4)))))


(defn straight? [hand]
  (let [ncheck (fn [ns] (== 4 (- (apply max ns) (apply min ns))))
       ranks (set (map rank hand))]
  (and (== 5 (count ranks))
       (or (ncheck ranks)
           (ncheck (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers [[high-card? 0] [pair? 1] [two-pairs? 2]
                  [three-of-a-kind? 3] [straight? 4] [four-of-a-kind? 4] 
                  [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]]
        res (map (fn [[ffn score]] (if (ffn hand) score 0)) checkers)]
    (apply max res)))
