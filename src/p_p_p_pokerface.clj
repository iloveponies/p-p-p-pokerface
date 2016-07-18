(ns p-p-p-pokerface)

(def charvals {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14})

(defn rank [[ch]]
  (if (Character/isDigit ch)
    (Integer/valueOf(str ch))
    (charvals ch)))

(defn suit [card]
  (str (second card)))

(defn ranks
  "Returns a sorted vector of ranks in given hand"
  [hand]
  (sort (map rank hand)))

(defn suits
  "Returns a sorted vector of suits in given hand"
  [hand]
  (sort (map suit hand)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (ranks hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (ranks hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (ranks hand)))) 4))

(defn flush? [hand]
  (== (count (set (suits hand)))
      1))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (== (or ((frequencies (vals (frequencies (ranks hand)))) 2) 0)
          2)))

(defn straight? [hand]
  (let [straight-helper (fn [ordered-ranks]
                          (= ordered-ranks (range (apply min ordered-ranks) (+ (apply min ordered-ranks) 5))))
        _ranks (ranks hand)]
    (or (straight-helper _ranks)
        (straight-helper (sort (replace {14 1} _ranks))))))


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
    (apply max (map (fn [[checker value]] ({true value false 0} (checker hand))) checkers))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
