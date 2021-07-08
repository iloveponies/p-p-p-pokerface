(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card] 
    (str suit)))

;; Return true of the hand has a pair
;; pair == two cards with same rank
(defn max-freq [hand]
  (apply max (vals (frequencies
                    (map rank hand)))))

(defn pair? [hand]
  (if (< 1 (max-freq hand))
      true
      false))

(defn three-of-a-kind? [hand]
  (if (< 2 (max-freq hand))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (< 3 (max-freq hand))
    true
    false))

(defn flush? [hand]
  (if (= 5 (apply max 
                  (vals (frequencies 
                         (map suit hand)))))
    true
    false))

;; This method gets the job done, but is awfully inefficient
(defn nth-freq [hand idx]
  (nth (reverse (sort (vals
                       (frequencies
                        (map rank hand))))) idx))

(defn full-house-ineff? [hand]
  (let [first (nth-freq hand 0)
        second (nth-freq hand 1)]
    (if (and (= 3 first) (= 2 second))
      true
      false)))

(defn full-house? [hand]
  (= (range 2 4) (filter (fn [fr]
                           (>= fr 2))
                         (sort (vals (frequencies
                                      (map rank hand)))))))

;; This can be done similar to prev function
;; But using 'range' should be simpler (and more efficient)
(defn two-pairs? [hand]
  (= '(2 2) (filter (fn [fr]
                     (>= fr 2)) 
                   (sort (vals (frequencies (map rank hand)))))))

(defn isConsecutive? [sorted-rank largest smallest]
  (= (range smallest (+ 1 largest)) sorted-rank))

(defn straight? [hand]
  (let [sorted-rank (sort (map rank hand))
        largest-rank (nth sorted-rank 4)
        smallest-rank (nth sorted-rank 0)]
    (if (isConsecutive? sorted-rank largest-rank smallest-rank)
      true
      (if (= 14 largest-rank)
        (let [replaced-rank (sort (replace {14 1} sorted-rank))]
          (if (isConsecutive? replaced-rank 5 1)
            true
            false))
        false))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
