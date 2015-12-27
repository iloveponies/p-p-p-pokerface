(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fir _] card]
  (cond
   (Character/isDigit fir) (Integer/valueOf(str fir))
   :else                   (replacements fir))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [cards (sort (vals (frequencies (map rank hand))))]
    (= cards (seq [2 3]))))

(defn two-pairs? [hand]
  (let [cards (sort (vals (frequencies (map rank hand))))]
    (or
     (= cards (seq [1 2 2]))
     (four-of-a-kind? hand))))

(defn straight? [hand]
  (or
    (let [[a b c d e] (sort (map rank hand))]
      (and (= (- e 4) a) (< a b c d e)))
    (let [[a b c d e] (sort (replace {14 1} (map rank hand)))]
      (and (= (- e 4) a) (< a b c d e)))))

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
    (apply max (map second (filter (fn [[fir _]] (fir hand)) checkers)))))
