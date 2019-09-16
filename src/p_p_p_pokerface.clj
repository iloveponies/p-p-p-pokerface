(ns p-p-p-pokerface)

(def card-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (card-ranks rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (if (= (apply max (vals (frequencies ranks))) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (= (apply max (vals (frequencies ranks))) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranks (into [] (map rank hand))]
    (if (= (apply max (vals (frequencies ranks))) 4)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (= (apply max (vals (frequencies suits))) 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (if (= (sort (vals (frequencies ranks))) (range 2 4))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (if (four-of-a-kind? hand)
      true
      (if (= (vals (frequencies ranks)) (list 2 2 1))
        true
        false))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        hmin (apply min ranks)
        hmax (apply max ranks)]
    (if (and (= hmin 2) (= hmax 14))
      (if (= (sort (replace {14 1} ranks)) (range 1 6))
        true
        false)
      (if (= ranks (range hmin (+ hmax 1)))
        true
        false))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        results []
        checker (fn [check] (if ((first check) hand)
                              (into results check)))
        filter-res (filter checkers (map checker checkers))]
    (apply max (map second filter-res))))
