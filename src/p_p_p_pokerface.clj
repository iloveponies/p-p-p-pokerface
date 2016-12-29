(ns p-p-p-pokerface)

(defn rank
  [card]
  (let [[fst _] card
        rnk (str fst)
        index {:T 10 :J 11 :Q 12 :K 13 :A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf rnk)
      ((keyword rnk) index))))

(defn suit
  [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair?
  [hand]
  (let [num-hand (count hand)
        ranks (map rank hand)
        unique-vals (-> ranks set count)]
    (< unique-vals num-hand)))

(defn three-of-a-kind?
  [hand]
  (let [ranks (map rank hand)
        max-freq (->> ranks
                      frequencies
                      vals
                      (apply max))]
    (<= 3 max-freq)))

(defn four-of-a-kind?
  [hand]
  (let [ranks (map rank hand)
        max-freq (->> ranks
                      frequencies
                      vals
                      (apply max))]
    (<= 4 max-freq)))

(defn flush?
  [hand]
  (let [suits (map suit hand)
        num-suits (-> suits set count)]
    (= 1 num-suits)))

(defn full-house?
  [hand]
  (let [ranks (map rank hand)
        sorted-freqs (-> ranks
                         frequencies
                         vals
                         sort)]
    (= sorted-freqs [2 3])))

(defn two-pairs?
  [hand]
  (let [ranks (map rank hand)
        sorted-freqs (-> ranks
                         frequencies
                         vals
                         sort)]
    (or (= sorted-freqs [1 2 2])
        (= sorted-freqs [1 4]))))

(defn straight?
  [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (set ranks))
        alt-ranks (sort (set (replace {14 1} ranks)))
        flush-hnd (range (first sorted-ranks)
                         (inc (last sorted-ranks)))
        alt-flush-hnd (range (first alt-ranks)
                             (inc (last alt-ranks)))]
    (or
     (and (= 5 (count sorted-ranks))
          (= sorted-ranks flush-hnd))
     (and (= 5 (count alt-ranks))
          (= alt-ranks alt-flush-hnd)))))

(defn straight-flush?
  [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value
  [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        filter-fn (fn [tuple]
                    (let [[matcher value] tuple]
                      (matcher hand)))
        matches (filter filter-fn checkers)
        values (map second matches)]
    (apply max values)))
