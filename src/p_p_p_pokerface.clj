(ns p-p-p-pokerface)

(defn rank [card]
  (let [crank (get card 0)]
    (if (Character/isDigit crank)
      (Integer/valueOf (str crank))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} crank))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-fs (frequencies ranks)
        fs (vals rank-fs)]
    (if (== (count (filter (fn [x] (== 2 x)) fs)) 1)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-fs (frequencies ranks)
        fs (vals rank-fs)]
    (if (and (not= (count (filter (fn [x] (== 2 x)) fs)) 1)
             (== (count (filter (fn [x] (== 3 x)) fs)) 1))
      true
      false)))

(defn four-of-a-kind? [hand]
   (let [ranks (map rank hand)
        rank-fs (frequencies ranks)
        fs (vals rank-fs)]
    (if (== (apply max fs) 4)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-fs (frequencies suits)
        fs (vals suit-fs)]
    (if (== (apply max fs) 5)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-fs (frequencies ranks)
        fs (vals rank-fs)]
    (if (and (== (count (filter (fn [x] (== 2 x)) fs)) 1)
             (== (count (filter (fn [x] (== 3 x)) fs)) 1))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-fs (frequencies ranks)
        fs (vals rank-fs)]
    (if (== (count (filter (fn [x] (== 2 x)) fs)) 2)
      true
      false)))

(defn straight? [hand]
  (let [ranks-orig (sort (mapv rank hand))
        ranks-rpl (sort (replace {14 1} (mapv rank hand)))
        [fi se th fo fiv] ranks-orig
        [fi1 se1 th1 fo1 fiv1] ranks-rpl]
    (if (or (and (== se (+ fi 1))
                 (== th (+ se 1))
                 (== fo (+ th 1))
                 (== fiv (+ fo 1)))
            (and (== se1 (+ fi1 1))
                 (== th1 (+ se1 1))
                 (== fo1 (+ th1 1))
                 (== fiv1 (+ fo1 1))))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [[f v]] (if (= (f hand) true) v -1)) checkers))))
