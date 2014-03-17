(ns p-p-p-pokerface)

;; Domain functions
(defn card [a-card]
  a-card)

(defn hand [a-hand]
  a-hand)

(defn rank [card]
  (let [[rank _] card
        rank-values {\1 1  \2 2  \3 3  \4 4
                     \5 5  \6 6  \7 7  \8 8
                     \9 9 \T 10 \J 11 \Q 12
                     \K 13 \A 14}]
    (rank-values rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;; Helper functions
(defn frequencies-of-rank [hand]
  (frequencies (map rank hand)))

(defn frequencies-of-suit [hand]
  (frequencies (map suit hand)))

;; Hand functions
(defn pair? [hand]
  ;; If there are only 4 elements left after grouping
  ;; the ranks there must be a pair
  (= (count (frequencies-of-rank hand))
     4))

(defn three-of-a-kind? [hand]
  ;; If there are only 3 elements left after grouping
  ;; the ranks there must be a set (three-of-a-kind)
  (= (apply max(vals (frequencies-of-rank hand)))
     3))

(defn four-of-a-kind? [hand]
  ;; If there is a tuple with a value of 4 there was a
  ;; four-of-a-kind
  (= (apply max(vals (frequencies-of-rank hand)))
     4))

(defn flush? [hand]
  ;; If all cards in the hand are of one suit there was
  ;; a flush
  (= (count (frequencies-of-suit hand))
     1))

(defn full-house? [hand]
  ;; If the resulting tuple values of a frequency count
  ;; are equal to [2 3] there was a fullhouse
  (= (sort (vals (frequencies-of-rank hand)))
     (seq [2 3])))

(defn two-pairs? [hand]
    (or
      (= (sort (vals (frequencies-of-rank hand)))
         (seq [1 2 2]))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let[sorted-rank (sort (keys (frequencies-of-rank hand)))
       first-rank (first sorted-rank)
       shifted-rank (map (fn [x] (- x first-rank)) sorted-rank)]
    (or (= shifted-rank [0 1 2 3 4])
        (= shifted-rank [0 1 2 3 12]))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

;; Value function
(defn value [hand]
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
