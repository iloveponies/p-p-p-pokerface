(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-map rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn slice-of-hand [hand slice]
  (map slice hand))

(defn hand-with-ace-check [hand]
  (let [hand (slice-of-hand hand rank)]
    (if (= (apply max hand) 14)
      (conj hand 1)
      hand)))

(defn of-a-rank? [hand n]
  (let [rank-hand (slice-of-hand hand rank)
        rank-frequencies (vals (frequencies rank-hand))
        ]
    (>= (apply max (vals (frequencies rank-hand))) n)))

(defn of-a-suit? [hand n]
  (let [suit-hand (slice-of-hand hand suit)
        rank-frequencies (vals (frequencies suit-hand))
        ]
    (>= (apply max (vals (frequencies suit-hand))) n)))

(defn pair? [hand]
  (of-a-rank? hand 2))

(defn three-of-a-kind? [hand]
  (of-a-rank? hand 3))

(defn four-of-a-kind? [hand]
  (of-a-rank? hand 4))

(defn flush? [hand]
  (of-a-suit? hand 5))

(defn sorted-val-freq [card-property hand]
  (sort (vals (frequencies (map card-property hand)))))

(defn full-house? [hand]
  (= '(2 3) (sorted-val-freq rank hand)))


(defn two-pairs? [hand]
  (or (= '(1 2 2) (sorted-val-freq rank hand))
      (= '(1 4) (sorted-val-freq rank hand))))

(defn straight? [hand]
  (let [hand (hand-with-ace-check hand)
        replace-map (zipmap hand (repeat (count hand) "x"))]
    (if (re-find #"xxxxx" (apply str (replace replace-map  (range 1 15))))
      true
      false)))

(defn straight-flush? [hand]
  (and
   (= (vals (frequencies (slice-of-hand hand suit))) '(5))
   (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        score-checker (fn [[check score]]
                        (if (check hand)
                          score
                          0))]
    (apply max (map score-checker checkers))))

