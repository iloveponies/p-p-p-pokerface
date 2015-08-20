(ns p-p-p-pokerface)

(defn suit [card]
  (str (second card)))

(defn rank [card]
  (cond
    (Character/isDigit (first card)) (Integer/valueOf (str (first card)))
    :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} (first card))))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (every? (fn [x] (= (suit (first hand)) (suit x))) hand))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (contains? (set (frequencies (vals (frequencies (map rank hand))))) [2 2]))

(defn straight-rank? [ranks]
  (= ranks (range (first ranks) (+ (first ranks) 5))))

(defn straight? [hand]
  (or (straight-rank? (sort (map rank hand)))
      (straight-rank? (sort (replace {14 1} (map rank hand))))))

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
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
