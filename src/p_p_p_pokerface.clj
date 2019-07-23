(ns p-p-p-pokerface)

(defn rank [[rank suit]]
  (let [ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Character/digit rank 10)
      (ranks rank))))

(defn suit [[rank suit]]
  (str suit))

(defn has-n-of-a-kind [n hand]
  (->> hand (map rank) frequencies vals (some #{n}) nil? not))

(defn pair? [hand]
   (has-n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (has-n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (has-n-of-a-kind 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (->> hand (map rank) frequencies vals (filter #{2}) count #{2} nil? not))

(defn straight? [hand]
  (let [ranks (->> hand (map rank))
        sorted-ranks (sort ranks)
        lowest (first sorted-ranks)
        alternative (sort (replace {14 1 1 14} ranks))
        alternative-lowest (first alternative)]
    (or (= sorted-ranks (range lowest (+ 5 lowest)))
        (= alternative (range alternative-lowest (+ 5 alternative-lowest))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [values #{[(fn [hand] true) 0]
                 [pair? 1]
                 [two-pairs? 2]
                 [three-of-a-kind? 3]
                 [straight? 4]
                 [flush? 5]
                 [full-house? 6]
                 [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter #((first %) hand)
                        values)))))
