(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ran _] card
        high-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit ran) (Integer/valueOf (str ran)) (high-ranks ran))))

(defn suit [card]
  (let [[_ sui] card]
    (str sui)))

(defn all-ranks [hand]
  (mapv rank hand))

(defn all-suits [hand]
  (mapv suit hand))

(defn n-of-a-kind? [hand how-many]
  (if (>= (apply max (vals (frequencies (all-ranks hand)))) how-many) true false))

(defn sorted-hand-frequencies [hand]
  (sort (vals (frequencies (all-ranks hand)))))


(defn replaced-sorted-hand [hand]
  (sort (replace {14 1} (all-ranks hand))))

(defn normal-sorted-hand [hand]
  (sort (all-ranks hand)))



(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))



(defn flush? [hand]
  (if (== (apply max (vals (frequencies (all-suits hand)))) 5) true false))


(defn full-house? [hand]
  (if (= (sorted-hand-frequencies hand) (range 2 4)) true false))

(defn two-pairs? [hand]
    (if (or (= (sorted-hand-frequencies hand) [1 2 2]) (= (sorted-hand-frequencies hand) [1 4])) true false))



(defn straight? [hand]
  (let [replaced-hand (replaced-sorted-hand hand)
        normal-hand (normal-sorted-hand hand)
        repl-min (apply min replaced-hand)
        repl-max (+ (apply max replaced-hand) 1)
        norm-min (apply min normal-hand)
        norm-max (+ (apply max normal-hand) 1)
        repl-range (range repl-min repl-max)
        norm-range (range norm-min norm-max)]
    (if (or (= replaced-hand repl-range) (= normal-hand norm-range)) true false)))



(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        remaining-checkers (filter (fn [checker] ((first checker) hand)) checkers)
        remaining-checker-values (map (fn [checker] (second checker)) remaining-checkers)]
    (apply max remaining-checker-values)))
