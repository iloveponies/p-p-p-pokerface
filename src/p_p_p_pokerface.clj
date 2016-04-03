(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn num-contained [hand num]
  (let [freqs (vals (frequencies (map rank hand)))]
    (filter (fn [val] (== val num)) freqs)))

(def not-empty? (complement empty?))

(defn pair? [hand]
  (not-empty? (num-contained hand 2)))

(defn three-of-a-kind? [hand]
  (not-empty? (num-contained hand 3)))

(defn four-of-a-kind? [hand]
  (not-empty? (num-contained hand 4)))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count (num-contained hand 2))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (or (= (range (apply min sorted) (inc (apply max sorted)))
           sorted)
        (let [new-hand (sort (replace {14 1} sorted))]
          (= (range (apply min new-hand) (inc (apply max new-hand)))
             new-hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [pred] ((first pred) hand)) checkers)))))
