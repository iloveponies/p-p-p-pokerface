(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))


(defn f-freqs [hand f]
  (sort
   (vals
    (frequencies
     (map f hand)))))

(defn rank-freqs [hand]
  (f-freqs hand rank))

(defn suit-freqs [hand]
  (f-freqs hand suit))

(defn max-count-of-rank [hand]
  (apply max (rank-freqs hand)))

(defn countfinder [hand exp-count]
  (fn [] (== (max-count-of-rank hand) exp-count)))

(defn pair? [hand]
  ((countfinder hand 2)))

(defn three-of-a-kind? [hand]
  ((countfinder hand 3)))

(defn four-of-a-kind? [hand]
  ((countfinder hand 4)))

(defn flush? [hand]
  (== (count (suit-freqs hand)) 1))

(defn full-house? [hand]
  (= (rank-freqs hand) [2 3]))

(defn two-pairs? [hand]
  (= (rank-freqs hand) [1 2 2]))

(defn in-order [hand]
  (let [ranks (map rank hand)
        max-rank (apply max ranks)
        min-rank (apply min ranks)]
    (= max-rank (+ min-rank 4))))

(defn straight? [hand]
  (and (= (rank-freqs hand) [1 1 1 1 1])
       (or (in-order hand)
           (in-order (replace {"AD" "1D"
                               "AH" "1H"
                               "AC" "1C"
                               "AS" "1S"} hand)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))


