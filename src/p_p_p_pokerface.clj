(ns p-p-p-pokerface)

(defn rank [card]
  (let [rankmap {\T 10 \J 11 \Q 12 \K 13 \A 14 }
        [r] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rankmap r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind [x hand]
  (= ((set (vals (frequencies (map rank hand)))) x) x))

(defn pair? [hand]
  (n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (= ((set (vals (frequencies (map suit hand)))) 5) 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= 2 (count (filter #(= 2 %) (vals (frequencies (map rank hand))))))))

(defn sorted-straight? [hand]
  (let [first-card (first hand)]
    (= hand (range first-card (+ first-card 5)))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        low-ace-sorted-hand (sort (replace {14 1} (map rank hand)))]
    (or (sorted-straight? sorted-hand)
        (sorted-straight? low-ace-sorted-hand))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (= (vals (frequencies (map suit hand))) [5])))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (last (sort (map (fn [[check? v]] (if (check? hand) v)) checkers)))))
