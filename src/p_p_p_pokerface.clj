(ns p-p-p-pokerface)

(def rank-replacements {\A 14
                        \K 13
                        \Q 12
                        \J 11
                        \T 10})

(defn rank [card]
 (let [[rank _] card]
  (if (Character/isDigit rank)
   (Integer/valueOf (str rank))
   (get rank-replacements rank))))

(defn suit [card]
 (let [[_ suit] card]
  (str suit)))

(defn n-of-a-kind [hand n]
 (let [rank-frequencies (frequencies (map rank hand))
       rank-vals (vals rank-frequencies)]
  (= n (apply max rank-vals))))

(defn pair? [hand]
 (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
 (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
 (n-of-a-kind hand 4))

(defn flush? [hand]
 (let [suits (map suit hand)
       suits-vals (vals (frequencies suits))]
  (=  5 (first suits-vals))))

(defn full-house? [hand]
 (let [rank-frequencies (frequencies (map rank hand))
       rank-vals (vals rank-frequencies)]
  (= (seq [2 3]) (sort rank-vals))))

(defn two-pairs? [hand]
 (let [rank-frequencies (frequencies (map rank hand))
       rank-vals (vals rank-frequencies)]
  (= (seq [1 2 2]) (sort rank-vals))))

(defn straight? [hand]
 (let [ranks (map rank hand)
       sorted-ranks (sort ranks)
       min-sorted (apply min ranks)]
  (or (= sorted-ranks (range min-sorted (+ min-sorted 5)))
      (= sorted-ranks [2 3 4 5 14]))))

(defn straight-flush? [hand]
 (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
 true)

(defn value [hand]
 (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
       checking-function (fn [checker] [((first checker) hand) (second checker)])
       mapped-checkers (map checking-function checkers)
       filtered-checkers (filter (fn [checker] (first checker)) mapped-checkers)
       scores (map second filtered-checkers)]
  (apply max scores)))
