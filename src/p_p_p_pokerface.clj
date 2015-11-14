(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {"T" 10
                "J" 11
                "Q" 12
                "K" 13
                "A" 14}
        [frs _] card]
    (if (Character/isDigit frs)
      (Integer/valueOf (str frs))
      (values (str frs)))))

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
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2]))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or
     (= ranks (range (apply min ranks) (+ 5 (apply min ranks))))
     (= (sort (replace {14 1} ranks)) (range 1 6)))))

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
    (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))
