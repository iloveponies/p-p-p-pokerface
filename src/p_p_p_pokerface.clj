(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank)))


(defn suit [[_ suit]]
  (str suit))


(defn n-of-a-kind? [n hand]
  (<= n (apply max (->>
     (map rank hand)
     (frequencies)
     (vals)
   ))))


(defn pair? [hand] (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn match-counts [hand]
  (vals (frequencies (map rank hand))))

(defn full-house? [hand]
  (= #{2 3} (set (match-counts hand))))

(defn two-pairs? [hand]
  (<= 4 (apply +
   (filter (fn [c] (>= c 2)) (match-counts hand)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        test
        (fn [ranks]
          (= (map (fn [r] (- r (apply min ranks))) ranks)
             '(0 1 2 3 4)))]
    (or (test ranks)
        (test (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
   (and (straight? hand) (flush? hand)))

(defn high-card? [_] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matches (filter (fn [[check _]] (check hand)) checkers)
        values (map second matches)]
    (apply max values)))
