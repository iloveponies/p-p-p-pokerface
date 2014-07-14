(ns p-p-p-pokerface)

(def replacement { \T 10 \J 11 \Q 12 \K 13 \A 14 })

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Character/getNumericValue rank)
    (replacement rank)))

(defn suit [[_ suit]]
  (str suit))

(defn repetitions [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (boolean (some #{2} (repetitions hand))))

(defn three-of-a-kind? [hand]
  (boolean (some #{3} (repetitions hand))))

(defn four-of-a-kind? [hand]
  (boolean (some #{4} (repetitions hand))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (repetitions hand)) [2 3]))

(defn two-pairs? [hand]
  (= (sort (repetitions hand)) [1 2 2]))

(defn adjacent? [list]
  (let [sorted (sort list)
        minimum (first sorted)]
    (= sorted (range minimum (+ minimum 5)))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (adjacent? ranks)
        (adjacent? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (keep #(if ((first %) hand) (second %)) checkers))))
