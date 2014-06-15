(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [hand n]
  (if (= n (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [ranks (map suit hand)]
    (if (= 5 (nth (vals (frequencies ranks)) 0))
      true
      false)))

(defn full-house? [hand]
  (if (= [2 3] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  (if (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn is-subsequent-collection [ranks i]
  (if (= i 4)
    true    
    (if (not= (+ (nth ranks i) 1) (nth ranks (+ i 1)))
      false
      (recur ranks (+ i 1)))
    ))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        replaced-hand (sort (replace {1 14} sorted-ranks))
        replaced-hand2 (sort (replace {14 1} sorted-ranks))]
    (or (is-subsequent-collection sorted-ranks 0)
        (is-subsequent-collection replaced-hand 0)
        (is-subsequent-collection replaced-hand2 0))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                 [two-pairs? 2] [three-of-a-kind? 3]
                 [straight? 4] [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        true-checkers (fn [e] (= ((first e) hand) true))
        found-values (map second (filter true-checkers checkers))]
    (apply max found-values)))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)