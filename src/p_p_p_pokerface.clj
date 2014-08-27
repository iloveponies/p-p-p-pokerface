(ns p-p-p-pokerface)

(defn card [a-card]
  a-card)

(defn hand [a-hand]
  a-hand)

(defn rank [card]
  (let [[rank _] card
        face-cards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face-cards rank))))

(defn suit [card]
   (let [[_ suit] card]
     (str suit)))

(defn ranks [hand]
  "Returns sequence of ranks in hand"
  (map (fn [card] (rank card)) hand))

(defn suits [hand]
  "Returns sequence of suits in hand"
  (map (fn [card] (suit card)) hand))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [counts (vals (frequencies (ranks hand)))]
    (= (apply max counts) 2)))

(defn three-of-a-kind? [hand]
(let [counts (vals (frequencies (ranks hand)))]
    (= (apply max counts) 3)))

(defn four-of-a-kind? [hand]
  (let [counts (vals (frequencies (ranks hand)))]
    (= (apply max counts) 4)))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (ranks hand)))) '(2 3)))

(defn two-pairs? [hand]
  "four of a kind is not two pairs but the tests make it so I guess"
  (let [counts (sort (vals (frequencies (ranks hand))))]
    (or (= counts '(1 2 2)) (= counts '(1 4)))))

;(defn high-straight? [hand]
;  (let [sorted-ranks (sort (ranks hand))
;        smallest (apply min sorted-ranks)
;        largest (apply max sorted-ranks)]
;    (= sorted-ranks (range smallest (+ 1 largest)))))

;(defn low-straight? [hand]
;  (let [sorted-ranks (sort (replace {14 1} (ranks hand)))
;        smallest (apply min sorted-ranks)
;        largest (apply max sorted-ranks)]
;   (= sorted-ranks (range smallest (+ 1 largest)))))

(defn straight? [hand]
  (let [consecutive-range? (fn [sorted-seq] (= sorted-seq (range (apply min sorted-seq) (+ (apply max sorted-seq) 1))))
        low-straight? (let [sorted-ranks (sort (replace {14 1} (ranks hand)))]
                        (consecutive-range? sorted-ranks))
        high-straight? (let [sorted-ranks (sort (ranks hand))]
                         (consecutive-range? sorted-ranks))]
    (or high-straight? low-straight?)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-hand (fn [checker] ((first checker) hand))
        passing-values (filter check-hand checkers)
        values (map second passing-values)]
    (apply max values)))
