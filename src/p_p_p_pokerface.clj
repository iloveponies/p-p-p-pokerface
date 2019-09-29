(ns p-p-p-pokerface)

(defn rank [card]
  (let [[R _] card]
    (get { \2 2 \3 3  \4 4  \5 5  \6 6  \7 7 \8 8
           \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14 }
         R)))

(defn suit [card]
  (let [[_ S] card] (str S)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freqs (set (vals (frequencies (map rank hand))))]
    (and (contains? freqs 2) (contains? freqs 3))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= 2 (count (filter (fn [f] (= f 2)) freqs)))))

(defn straight? [hand]
  (let [a (sort (map rank hand))
        b (sort (replace {14 1} (map rank hand)))
        test? (fn [seq] (= seq (range (first seq) (+ 5 (first seq)))))]
    (or (test? a) (test? b))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
        value (fn [p] (if ((first p) hand) (second p) 0))]
    (apply max (map value checkers)))) 
