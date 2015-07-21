(ns p-p-p-pokerface)

(defn rank [card]
  (def rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank suit] card]
    (cond
       (Character/isDigit rank) (Integer/valueOf (str rank))
       :else (get rank-map rank))))

(defn suit [card]
  (let [[rank suit] card]
       (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
    (or (= freq [1 2 2]) (= freq [1 4]))))

(defn straight? [hand]
  (let [freq (sort (map rank hand))
        mini (apply min freq)
        maxi (inc (apply max freq))]
    (or (= freq (range mini maxi)) (= freq [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[fn score]] 
                          (fn hand)) checkers)))))
