(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-char (get card 0)]
    (cond
     (Character/isDigit rank-char) (Integer/valueOf (str rank-char))
     (= \T rank-char) 10
     (= \J rank-char) 11
     (= \Q rank-char) 12
     (= \K rank-char) 13
     (= \A rank-char) 14)))

(defn count-same-ranks [rank-count hand]
  (count (filter (fn [x] (= x rank-count)) (vals (frequencies (map rank hand))))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (= 1 (count-same-ranks 2 hand)))

(defn three-of-a-kind? [hand]
  (= 1 (count-same-ranks 3 hand)))

(defn four-of-a-kind? [hand]
  (= 1 (count-same-ranks 4 hand)))

(defn flush? [hand]
  (= 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count-same-ranks 2 hand)))

(defn straight? [hand]
  (let [rank-seq (sort (map rank hand))
        rank-seq-r (sort (replace {14 1} (map rank hand)))]
    (or (= (range (apply min rank-seq) (+ 5 (apply min rank-seq)))
           rank-seq)
        (= (range (apply min rank-seq-r) (+ 5 (apply min rank-seq-r)))
           rank-seq-r))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checked (map (fn [[checker score]] [(checker hand) score])
                     checkers)
        valid-hand (filter (fn [checked] (first checked)) checked)
        eval-hand (map second valid-hand)]
    (apply max eval-hand)))
