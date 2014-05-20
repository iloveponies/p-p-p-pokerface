(ns p-p-p-pokerface)

(def ranks {"2" 2
            "3" 3
            "4" 4
            "5" 5
            "6" 6
            "7" 7
            "8" 8
            "9" 9
            "T" 10
            "J" 11
            "Q" 12
            "K" 13
            "A" 14})

(defn rank [[r _]]
  (ranks (str r)))

(defn suit [[_ s]]
  (str s))

(defn- times-of-a-kind [num hand]
  (= num (apply max
     (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (times-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (times-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (times-of-a-kind 4 hand))

(defn flush? [hand]
  (= 5 (apply max
     (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(2 3)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [cards (sort (vals (frequencies (map rank hand))))]
    (or (= '(1 2 2) cards)
        (= '(1 4) cards))))

(defn- range-of-five? [card-ranks]
  (let [min-rank (first card-ranks)
        max-rank (last card-ranks)]
    (= card-ranks (range min-rank (+ min-rank 5)))))

(defn- low-ace-card-straight? [card-ranks]
  (let [low-ace-card-ranks (sort (replace {14 1} card-ranks))]
    (range-of-five? low-ace-card-ranks)))

(defn straight? [hand]
  (let [card-ranks (sort (map rank hand))]
    (or (range-of-five? card-ranks)
        (low-ace-card-straight? card-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn- high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter #((first %) hand) checkers)))))
