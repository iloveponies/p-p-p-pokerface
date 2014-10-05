(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card
        face {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (not (Character/isDigit r))
      (get face r)
      (Integer/valueOf (str r)))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (not
   (empty?
    (filter (fn [r] (= r 2))
            (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not
   (empty?
    (filter (fn [r] (= r 3))
            (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not
   (empty?
    (filter (fn [r] (= r 4))
            (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or ( = (count (filter (fn [r] (= r 2))
                         (vals (frequencies (map rank hand)))))
          2)
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        high-ace-sort (sort ranks)
        low-ace-sort (sort (replace {14 1} ranks))
        first-high (first high-ace-sort)
        first-low (first low-ace-sort)]
    (or
     (= high-ace-sort (range first-high (+ first-high 5)))
     (= low-ace-sort (range first-low (+ first-low 5))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        is-hand? (fn [entry] (if ((first entry) hand)
                              (second entry)
                              0))]
    (apply max (map is-hand? checkers))))
