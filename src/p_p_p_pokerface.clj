(ns p-p-p-pokerface)

(defn rank [[fst snd]]
  (let [charmap {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14}]
    (get charmap fst)))

(defn suit [[fst snd]]
  (str snd))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 4] freqs) (= [1 2 2] freqs))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        minrank (apply min ranks)]
    (or (= ranks (range minrank (+ minrank 5))) (= ranks [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-value (fn [[checker value]]
          (if (checker hand) value 0))]
    (apply max (map hand-value checkers))))
