(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        value {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank) (Integer/valueOf (str rank)) (get value rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [card-freqs (vals (frequencies (map rank hand)))]
    (= (apply max card-freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [card-freqs (vals (frequencies (map rank hand)))]
    (= (apply max card-freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [card-freqs (vals (frequencies (map rank hand)))]
    (= (apply max card-freqs) 4)))

(defn flush? [hand]
  (let [card-freqs (vals (frequencies (map suit hand)))]
    (>= (apply max card-freqs) 5)))

(defn full-house? [hand]
  (let [card-freqs (set (vals (frequencies (map rank hand))))]
    (= (set [2 3]) card-freqs)))

(defn two-pairs? [hand]
  (let [card-freqs (sort (vals (frequencies (map rank hand))))]
    (= [1 2 2] card-freqs)))

(defn straight? [hand]
  (let [vals1 (sort (map rank hand)) vals2 (sort (replace {14, 1} vals1))
        straightvals (range (apply min vals1) (+ (apply max vals1) 1))
        straightvals2 (range (apply min vals2) (+ (apply max vals2) 1))]
    (or (= straightvals vals1) (= straightvals2 vals2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [card]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))

