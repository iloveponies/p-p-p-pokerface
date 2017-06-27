(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (let [court->integer {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (court->integer rank)))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 4)
    true
    false))

(defn flush? [hand]
(if (= (apply max (vals (frequencies (map suit hand)))) 5)
  true
  false))

(defn full-house? [hand]
(if (= [2 3] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  (cond
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))  true
    (four-of-a-kind? hand)                                   true
    :else                                                    false))

(defn straight? [hand]
  (let [ace-as-14     (fn [hand] (map rank hand))
        ace-as-one    (fn [hand] (replace {14 1} (map rank hand)))
        equals-range? (fn [hand] (= (sort hand)
                         (range (apply min hand)
                                (+ (apply max hand) 1))))]
  (cond (equals-range? (ace-as-14 hand))                     true
        (and (== (apply max (ace-as-14 hand)) 14)
             (not (equals-range? (ace-as-14 hand))))         (equals-range? (ace-as-one hand))
        :else                                                false)))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true
    false))

(defn value [hand]
  nil)
