(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[rnk _] card
        char-int {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get char-int rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn unique-ranks [hand]
  (count (frequencies (map rank hand))))

(defn unique-suits [hand]
  (count (frequencies (map suit hand))))

(defn num-of-pairs [hand]
  (let [ranks (frequencies (map rank hand))]
    (count (filter #{2} (vals ranks)))))

(defn pair? [hand]
    (< 0 (num-of-pairs hand)))

(defn three-of-a-kind? [hand]
    (and (== (unique-ranks hand) 3) (not (pair? hand))))

(defn four-of-a-kind? [hand]
    (and (== (unique-ranks hand) 2) (not (pair? hand))))

(defn flush? [hand]
  (== (unique-suits hand) 1))

(defn full-house? [hand]
  (and (== (unique-ranks hand) 2) (== (num-of-pairs hand) 1)))

(defn two-pairs? [hand]
  (or (== (num-of-pairs hand) 2) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        swap-ace? (== (apply min ranks) 2)
        ranks (if swap-ace? (replace {14 1} ranks) ranks)
        sorted-ranks (sort ranks)
        min-in-hand (apply min sorted-ranks)]
    (= sorted-ranks (range min-in-hand (+ min-in-hand 5)))))

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
        give-points (fn [x] (let [[fun, points] x] (if (fun hand) points 0)))
        points (map give-points checkers)]
    (apply max points)))

