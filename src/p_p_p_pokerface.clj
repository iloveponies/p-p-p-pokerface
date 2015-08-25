(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rojals {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rojals fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [n hand]
  (.contains
    (vals
      (frequencies
        (map rank hand))) n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (n-of-a-kind? 2 hand)
       (n-of-a-kind? 3 hand)))

(defn two-pairs? [hand]
  (let [kinds (vals (frequencies (map rank hand)))]
    (or (= [1 2 2] (sort kinds))
        (full-house? hand)
        (n-of-a-kind? 4 hand))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        [f] sorted]
    (or (= sorted (range f (+ f 5)))
        (= sorted [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-value (fn [checker] [((first checker) hand)
                                 (second checker)])
        values (filter first (map check-value checkers))]
    (apply max (map second values))))
