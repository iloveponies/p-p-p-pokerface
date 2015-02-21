(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< (count(set (map rank hand))) (count hand)))

(defn three-of-a-kind? [hand]
  (> (count (filter (fn [x] (>= x 3)) (vals
   (frequencies (map rank hand))))) 0))

(defn four-of-a-kind? [hand]
  (> (count (filter (fn [x] (>= x 4)) (vals
   (frequencies (map rank hand))))) 0))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (let [shand (sort hand)
        fcard (rank (first shand))]
    (and
     (three-of-a-kind? (filter (fn [x] (== (rank x) fcard)) shand))
     (pair? (filter (fn [x] (not (== (rank x) fcard))) shand)))))


(defn two-pairs? [hand]
  (let [shand (sort hand)
        fcard (rank (first shand))]
    (and
     (pair? (filter (fn [x] (== (rank x) fcard)) shand))
     (pair? (filter (fn [x] (not (== (rank x) fcard))) shand)))))

(defn straight? [hand]
  (let [shand (sort (map rank hand))
        comp-cards (fn [h] (= h (range (first h) (+ (first h) 5))))]
    (or (comp-cards shand) (comp-cards (sort ( replace {14 1} shand))))))

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
  (apply max
         (map second
              (filter
               (fn [checker]
                 ( (first checker) hand)) checkers)))))
