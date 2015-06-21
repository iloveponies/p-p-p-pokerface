(ns p-p-p-pokerface)

(def ranks-in-numbers {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card ]
    (if (Character/isDigit rank) (Integer/valueOf (str rank))(get ranks-in-numbers rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequencies-of-ranks [hand]
  (vals (frequencies (map rank hand))))


(defn amount-of-most-frequent-rank [hand]
  (apply max (frequencies-of-ranks hand)))

(defn pair? [hand]
  (if (>= (amount-of-most-frequent-rank hand) 2)
    true false))

(defn three-of-a-kind? [hand]
 (if (>= (amount-of-most-frequent-rank hand) 3)
    true false))

(defn four-of-a-kind? [hand]
  (if (>= (amount-of-most-frequent-rank hand) 4)
    true false))

(defn frequencies-of-suits [hand]
  (count (vals(frequencies(map suit hand)))))

(defn flush? [hand]
  (if (= 1 (frequencies-of-suits hand))
    true false))

(defn full-house? [hand]
  (= [3 2] (frequencies-of-ranks hand)))

(defn two-pairs? [hand]
  (or (= [2 2 1] (frequencies-of-ranks hand))
      (= [4 1] (frequencies-of-ranks hand))))

(defn straight? [hand]
    (let [ace-as-14 (map rank hand)
        ace-as-1 (replace {14 1} ace-as-14)
        is-straight? (fn [ranked-hand]
                       (and (= (count ranked-hand) 5)
                            (= 1 (apply max (vals (frequencies ranked-hand))))
                            (= (- (apply max ranked-hand) 4)
                               (apply min ranked-hand))))
        ]
    (or (is-straight? ace-as-1)
        (is-straight? ace-as-14))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
    (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
