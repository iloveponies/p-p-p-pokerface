(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freqs-vals (vals (frequencies (map rank hand)))]
    (and (= (apply max freqs-vals) 3)
         (= (apply min freqs-vals) 2))))

(defn two-pairs? [hand]
  (let [freqs-vals (vals (frequencies (map rank hand)))]
    (cond
      (four-of-a-kind? hand) true
      :else (= (get (frequencies freqs-vals) 2) 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low (apply min ranks)]
    (or (= (sort ranks)
           (range low (+ low 5)))
        (= (sort (replace {14 1} ranks))
           (range 1 6)))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter
                  (fn [checker]
                    ((first checker) hand))
                  checkers)))))

