(ns p-p-p-pokerface)

(defn
  suit
  [card]
  (let [[_ card-suit] card]
      (str card-suit)))

(defn
  rank
  [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [card-rank _] card]
          (if (Character/isDigit card-rank)
            (Integer/valueOf (str card-rank))
            (replacements card-rank))))

(defn frequencies-of-hand [funktion hand]
  (vals (frequencies (map funktion hand))))

(defn largest-freq [funktion hand]
  (apply max (frequencies-of-hand funktion hand)))

(defn
  pair?
  [hand]
  (= (largest-freq rank hand) 2))

(defn
  three-of-a-kind?
  [hand]
  (= (largest-freq rank hand) 3))

(defn
  four-of-a-kind?
  [hand]
  (= (largest-freq rank hand) 4))

(defn
  flush?
  [hand]
  (= (largest-freq suit hand) 5))

(defn
  full-house?
  [hand]
  (= [2 3] (sort (frequencies-of-hand rank hand))))

(defn
  two-pairs?
  [hand]
  (= [1 2 2] (sort (frequencies-of-hand rank hand))))

(defn
  straight?
  [hand]
    (let [ranks (map rank hand)
          sorted-ranks (sort ranks)
          largest-rank (apply max sorted-ranks)
          smallest-rank (apply min sorted-ranks)
          range-between-smallest-and-largest (range smallest-rank (+ 1 largest-rank))]
      (if (= (count range-between-smallest-and-largest) 13)
        true
        (= range-between-smallest-and-largest sorted-ranks))))

(defn
  straight-flush?
  [hand]
  (and (flush? hand) (straight? hand)))

(defn
  value
  [hand]
  (cond
	  (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
