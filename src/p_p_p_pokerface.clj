(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rk _] card]
    (if (Character/isDigit rk)
      (Integer/valueOf (str rk))
      (let [char-reprs {\T 10, \J 11, \Q, 12, \K 13, \A 14}]
        (get char-reprs rk)))))

(defn suit [card]
  (let [[_ st] card] (str st)))

;; The largest amount there's cards of same rank in a hand
(defn max-ranks [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (== 2 (max-ranks hand)))

(defn three-of-a-kind? [hand]
  (== 3 (max-ranks hand)))

(defn four-of-a-kind? [hand]
  (== 4 (max-ranks hand)))

(defn flush? [hand]
  (== 1 (count (set (map suit hand)))))

;; This helper function gives hand's ranks' freq numbers
(defn rank-freq-vals [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (= (rank-freq-vals hand) [2 3]))

(defn two-pairs? [hand]
  (or (= (rank-freq-vals hand) [1 2 2])
      (= (rank-freq-vals hand) [1 4])))

;; If ranks are 'straight'? (ace considered 14 as usual)
(defn straight-no-ace? [ranks]
  (if (> (count (frequencies ranks)) 1)
    (let [hmax (apply max ranks)
          hmin (apply min ranks)]
      (= (range hmin (+ hmax 1)) (sort ranks)))
    false))

(defn straight? [hand]
  (or (straight-no-ace? (map rank hand))
      (straight-no-ace? (replace {14 1} (map rank hand)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check (fn [[question? _]]
                (question? hand))]
    (apply max (map second (filter check checkers)))))
