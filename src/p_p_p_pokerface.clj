(ns p-p-p-pokerface)

(defn rank [card]
  (let [[v _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit v)
      (Integer/valueOf (str v))
      (values v))))

(defn such-ranks [hand]
  (vals (frequencies (map rank hand))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn such-suit [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (contains? (set (such-ranks hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (such-ranks hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (such-ranks hand)) 4))

(defn flush? [hand]
  (contains? (set (such-suit hand)) 5))

(defn full-house? [hand]
  (= [2 3] (sort (set (such-ranks hand)))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (such-ranks hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        aranks (sort (replace {14 1} ranks))
        shouldbe (range (apply min ranks) (+ (apply min ranks) 5))
        ashouldbe (range (apply min aranks) (+ (apply min aranks) 5))]
    (or (= ranks shouldbe)
        (= aranks ashouldbe))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
