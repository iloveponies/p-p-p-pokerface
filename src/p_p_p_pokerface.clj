(ns p-p-p-pokerface)

(defn rank [[rnk _]]
  (if (Character/isDigit rnk)
    (Integer/valueOf (str rnk))
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rnk)))

(defn suit [[_ s]]
  (str s))


(comment
  (def high-seven ["2H" "3S" "4C" "5C" "7D"])
  (def pair-hand ["2H" "2S" "4C" "5C" "7D"])
  (def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
  (def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
  (def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
  (def straight-hand ["2H" "3S" "6C" "5D" "4D"])
  (def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
  (def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
  (def flush-hand ["2H" "4H" "5H" "9H" "7H"])
  (def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
  (def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
  (def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
  (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
  )


; -------------------------------------
; my helpers

(defn rank-freqs [hand]
  (vals (frequencies (map rank hand))))

; apply - merges seqs into tuple fed to max fn
(defn frequency-of-kind [hand kind-freq]
  (= kind-freq (apply max (rank-freqs hand))))

(defn frequencies-match? [hand freqs]
  (= (sort (rank-freqs hand)) (sort freqs)))

(defn ranks-are-sequential? [hand-ranks]
  (let [sorted-ranks (sort hand-ranks)
        first-rank (first sorted-ranks)
        ranks-range (range first-rank (+ first-rank 5))]
    (= sorted-ranks ranks-range)))

(defn ranks [hand]
  (map rank hand))

(defn sequential-ranks-with-aces? [hand]
  (let [hand-ranks (ranks hand)]
    (or
      (ranks-are-sequential? hand-ranks)
      (ranks-are-sequential? (replace {14 1} hand-ranks)))))

; -------------------------------------

(defn pair? [hand]
  (frequency-of-kind hand 2))

(defn three-of-a-kind? [hand]
  (frequency-of-kind hand 3))

(defn four-of-a-kind? [hand]
  (frequency-of-kind hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (frequencies-match? hand '(2 3)))

(defn two-pairs? [hand]
  (or
    (frequencies-match? hand '(1 2 2))
    (frequencies-match? hand '(1 4))))

(defn straight? [hand]
  (let [at-least-two-suits? (fn [hnd] (> (count (set (map suit hand))) 1))]
    (and
      (at-least-two-suits? hand)
      (sequential-ranks-with-aces? hand))))

(defn straight-flush? [hand]
  (let [same-suit? (fn [hnd] (= (count (set (map suit hand))) 1))]
    (and
      (same-suit? hand)
      (sequential-ranks-with-aces? hand))))

(defn high-card? [hand]
  true) ; All hands have a high card.

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
    (apply max (map
                 (fn [checker]
                   (let [[checker-fn val] checker]
                     (if (apply checker-fn [hand]) val 0)))
                 checkers))))
