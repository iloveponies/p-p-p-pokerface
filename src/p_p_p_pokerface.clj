(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-to-int {"2" 2
                     "3" 3
                     "4" 4
                     "5" 5
                     "6" 6
                     "7" 7
                     "8" 8
                     "9" 9
                     "T" 10
                     "J" 11
                     "Q" 12
                     "K" 13
                     "A" 14}
        [r _] card]
    (get rank-to-int (str r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn n-of-a-kind? [hand n ]
  (let [ranks (map rank hand)
        ranks-counts (set (vals (frequencies ranks)))]
    (contains? ranks-counts n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        ranks-counts (set (vals (frequencies ranks)))]
    (= (range 2 4) (sort ranks-counts))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        ranks-counts (set (vals (frequencies ranks)))]
    (or (= [2 2] (sort ranks-counts)) (= ranks (four-of-a-kind? hand)))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
