(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank) (Integer/valueOf (str rank)) (get rank-map rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (boolean (some #{2} (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (boolean (some #{3} (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (boolean (some #{4} (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= (count (distinct (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [ranks  (map rank hand)
        sorted (sort (if (= (apply min ranks) 2) (replace {14 1} ranks) ranks))
        fst    (apply min sorted)
        refe   (range fst (+ fst 5))]
    (= sorted refe)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]},
        active (filter (fn [checker] ((first checker) hand)) checkers)
        scores (map second active)]
    (apply max scores)))
