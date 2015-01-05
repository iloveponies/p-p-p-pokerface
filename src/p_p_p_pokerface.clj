(ns p-p-p-pokerface)

(defn rank [card]
  (let [ [r _] card
         dic {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get dic r))))

(defn suit [card]
  (let [ [_ s] card]
    (str s)))

(defn same-rank [hand]
  (vals (frequencies (map rank hand))))

(defn max-same-rank [hand]
  (apply max (same-rank hand)))

(defn max-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn pair? [hand]
  (== 2 (max-same-rank hand)))

(defn three-of-a-kind? [hand]
  (== 3 (max-same-rank hand)))

(defn four-of-a-kind? [hand]
  (== 4 (max-same-rank hand)))

(defn flush? [hand]
  (== 5 (max-same-suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (same-rank hand))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= [1 2 2] (sort (same-rank hand)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        one-based-ranks (replace {14 1} ranks)
        change-base (fn [ranks] (map (fn [x] (- x (first ranks))) ranks))
        zero-to-4 (range 5)]
    (or (= zero-to-4 (change-base (sort ranks)))
        (= zero-to-4 (change-base (sort one-based-ranks))))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (map second (filter (fn [[f _]] (f hand)) checkers))]
    (apply max values)))


