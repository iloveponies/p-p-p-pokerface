(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (cond
     (= \T fst) 10
     (= \J fst) 11
     (= \Q fst) 12
     (= \K fst) 13
     (= \A fst) 14
     :else (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn rank-freq [hand]
  (vals (frequencies (map rank hand))))

(defn max-freq [hand]
  (apply max (rank-freq hand)))

(defn pair? [hand]
   (= 2 (max-freq hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-freq hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-freq hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (sort (rank-freq hand)) (range 2 4)))

(defn two-pairs? [hand]
  (= (sort (rank-freq hand)) (seq [1 2 2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        start (apply min ranks)
        end (+ start 5)
        acecase [2 3 4 5 14]]
   (or (= acecase ranks) (= (range start end) ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (= (max-freq hand) 1))

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
    (apply max
           (map (fn [checker]
                  (if ((first checker) hand)
                    (second checker)
                    0)) checkers))))
