(ns p-p-p-pokerface)

(def m-suit
{\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn suit [[r s]]
  (str s))

(defn rank [[r s]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get m-suit r)))

(defn repeats-rank? [hand times]
  (= times (apply max (vals (frequencies (map rank hand))))))

(defn repeats-suit? [hand times]
  (= times (apply max (vals (frequencies (map suit hand))))))

(defn pair? [hand]
  (repeats-rank? hand 2))

(defn three-of-a-kind? [hand]
  (repeats-rank? hand 3))

(defn four-of-a-kind? [hand]
  (repeats-rank? hand 4))

(defn flush? [hand]
  (repeats-suit? hand 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [r1 (sort (map rank hand))
        r2 (sort (replace {14 1} r1))
        min-r1 (apply min r1)
        min-r2 (apply min r2)]
    (or
     (= (range min-r1 (+ 5 min-r1)) r1)
     (= (range min-r2 (+ 5 min-r2)) r2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        trues (filter #((first %) hand) checkers)
        values (cons 0  (map second trues))]
    (apply max values)))
