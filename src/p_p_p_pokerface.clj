(ns p-p-p-pokerface)

(def letter-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get letter-ranks rank)))

(defn suit [[_ suit]]
  (str suit))

(defn ^:private sorted-ranks [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn ^:private hand-ranks? [ranks hand]
  (= (sorted-ranks hand) ranks))

(defn ^:private n-of-a-kind? [n hand]
  (>= (last (sorted-ranks hand)) n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (hand-ranks? [2 3] hand))

(defn two-pairs? [hand]
  (or (hand-ranks? [1 2 2] hand) (hand-ranks? [1 4] hand)))

(defn straight? [hand]
  (let [straight-ranks? (fn [ranks]
                          (let [sorted (sort ranks)
                                low (first sorted)
                                range (range low (+ low 5))]
                            (= sorted range)))
        high-ace-ranks (map rank hand)
        low-ace-ranks (replace {14 1} high-ace-ranks)]
    (or (straight-ranks? high-ace-ranks)
        (straight-ranks? low-ace-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers [straight-flush?
               four-of-a-kind?
               full-house?
               flush?
               straight?
               three-of-a-kind?
               two-pairs?
               pair?
               high-card?])

(defn value [hand]
  (loop [to-check checkers]
    (if ((first to-check) hand)
      (dec (count to-check))
      (recur (rest to-check)))))
