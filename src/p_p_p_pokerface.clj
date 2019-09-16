(ns p-p-p-pokerface)

(defn rank [card]
  (let [
        high-values {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r _] card
        is-digit (Character/isDigit r)
        ]
    (if is-digit
      (Integer/valueOf (str r))
      (high-values r)
      )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn at-least [hand n]
  (let [filtered (filter (fn [x] (< (- n 1) x)) (vals (frequencies (map rank hand))))]
    (cond
     (empty? filtered) false
     (== 1 (count filtered)) (== n (first filtered))
     :else (== n (apply min filtered)))))

(defn pair? [hand]
  (at-least hand 2))

(defn three-of-a-kind? [hand]
  (at-least hand 3))

(defn four-of-a-kind? [hand]
  (at-least hand 4))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
   (let [pfreq ((frequencies (sort (vals (frequencies (map rank hand))))) 2)]
     (or
      (and (boolean pfreq) (boolean (== 2 pfreq)))
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [
        sorted14 (sort (map rank hand))
        sorted1 (sort (replace {14 1} sorted14))
        min14 (apply min sorted14)
        max14 (+ 1 (apply max sorted14))
        min1 (apply min sorted1)
        max1 (+ 1 (apply max sorted1))
        expected14 (range min14 max14) 
        expected1 (range min1 max1) 
        ]
    (or
     (= expected14 sorted14)
     (= expected1 sorted1))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checked-value (fn [checker] (if ((first checker) hand) (second checker) 0))
        checked-values (map checked-value checkers)]
    (apply max checked-values )))
