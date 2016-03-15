(ns p-p-p-pokerface)

(def high-ace-ranks {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14})

(def low-ace-ranks {\T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 1})

(defn rank-using [card ranks-map]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get ranks-map rank))))

(defn rank [card]
  (rank-using card high-ace-ranks))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn raw-ranks [hand]
  (let [rank (fn [card]
               (let [[rank suit] card]
                 rank))]
    (map rank hand)))

(defn n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        max-freq (apply max (vals (frequencies ranks)))]
    (>= max-freq n)))
(defn high-card? [hand]
  true)

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        max-freq (apply max (vals (frequencies suits)))]
    (>= max-freq 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [1 2 2] (sort freqs))))

(defn straight? [hand]
  (let [ranks (raw-ranks hand)
        low-ace-ranks (sort (map (fn [card] (rank-using card low-ace-ranks)) hand))
        high-ace-ranks (sort (map rank hand))
        min-max-range (fn [seq] (range (apply min seq) (+ (apply max seq) 1)))]
    (or (= (min-max-range low-ace-ranks) low-ace-ranks)
        (= (min-max-range high-ace-ranks) high-ace-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        match (filter (fn [check-pair] (let [checker (first check-pair)]
                                           (checker hand))) checkers)
        values (map second match)]
    (apply max values)))
