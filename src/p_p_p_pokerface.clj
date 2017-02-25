(ns p-p-p-pokerface)

(defn rank [[rank-char _]]
  (let [rank-vals {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-vals rank-char))))

(defn suit [[_ card-suit]]
  (str card-suit))

(defn n-of-a-kind [n]
  (fn [hand]
    (boolean (some #{n} (vals (frequencies (map rank hand)))))))

(defn hand-rank-freqs [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn hand-ranks [hand]
  (sort (map rank hand)))

(def pair?
  (n-of-a-kind 2))

(def three-of-a-kind?
  (n-of-a-kind 3))

(def four-of-a-kind?
  (n-of-a-kind 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (hand-rank-freqs hand) [2 3]))

(defn two-pairs? [hand]
  (boolean (#{[1 2 2] [1 4]} (hand-rank-freqs hand))))

(defn straight? [hand]
  (let [sorted-ranks (hand-ranks hand)
        sorted-ranks' (sort
                       (replace {14 1} sorted-ranks))
        sequential (fn [first-rank] (range first-rank
                                           (+ first-rank 5)))]
    (or (= sorted-ranks
           (sequential (first sorted-ranks)))
        (= sorted-ranks'
           (sequential (first sorted-ranks'))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers [[straight-flush? 8] [four-of-a-kind? 7]
                  [full-house? 6] [flush? 5]
                  [straight? 4] [three-of-a-kind? 3]
                  [two-pairs? 2] [pair? 1]
                  [high-card? 0]]]
    (some #(if ((first %) hand) (second %)) checkers)))
