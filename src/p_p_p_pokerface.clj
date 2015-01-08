(ns p-p-p-pokerface)

(def card-values {\A 14, \K 13, \Q 12, \J 11, \T 10})


(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (card-values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(suit "5H")
(rank "5H")

(defn card-rank-sequences [hand]
  (vals (frequencies (map (fn [card] (rank card)) hand))))

(defn highest-rank-sequence [hand]
  (apply max (card-rank-sequences hand)))

(defn pair? [hand]
 (= 2 (highest-rank-sequence hand)))

(defn three-of-a-kind? [hand]
  (= 3 (highest-rank-sequence hand)))

(defn four-of-a-kind? [hand]
  (= 4 (highest-rank-sequence hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies
                         (map (fn [card]
                                (suit card))
                              hand))))))

(defn full-house? [hand]
  (let [cards (reverse (sort (card-rank-sequences hand)))]
    (and
     (= 3 (Integer/valueOf (str (first cards))))
     (= 2 (Integer/valueOf (str (second cards)))))))

(defn two-pairs? [hand]
(let [cards (reverse (sort (card-rank-sequences hand)))]
    (and
     (= 2 (Integer/valueOf (str (first cards))))
     (= 2 (Integer/valueOf (str (second cards)))))))


(defn straight? [hand]
  (let [high-ace-ranks (map rank hand)
        low-ace-ranks (replace {14 1} high-ace-ranks)
        series? (fn [ranks] (let [sorted (sort ranks)
                      start (first sorted)]
                              (= sorted (range start (+ start 5)))
                  ))]
    (or (series? high-ace-ranks) (series? low-ace-ranks))))

(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers [[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]]]
    (apply max (map (fn [checker] (let [[func value] checker]
                         (if(func hand)
                           value
                           0))) checkers))))









