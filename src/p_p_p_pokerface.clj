(ns p-p-p-pokerface)

(def face-cards {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (face-cards r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (if (some #(= 2 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (some #(= 3 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (some #(= 4 %) (vals (frequencies (map rank hand))))
    true
    false))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks-acehi (sort (map rank hand))
        ranks-acelo (sort (replace {14 1} ranks-acehi))]
    (or
     (= ranks-acehi (range (first ranks-acehi) (+ 1 (last ranks-acehi))))
     (= ranks-acelo (range (first ranks-acelo) (+ 1 (last ranks-acelo)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
