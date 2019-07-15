(ns p-p-p-pokerface)


(defn rank [card]
  (let [[ra _] card
        rank-string (str ra)
        repla {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit ra)
      (Integer/valueOf rank-string)
      (repla ra))))


(defn suit [card]
  (let [[_ su] card]
    (str su)))


(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        massimo (apply max (vals freq))]
    (= massimo 2)))


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        massimo (apply max (vals freq))]
    (= massimo 3)))


(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        massimo (apply max (vals freq))]
    (= massimo 4)))


(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)
        massimo (apply max (vals freq))]
    (= massimo 5)))


(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        massimo (apply max (vals freq))
        minimo (apply min (vals freq))]
    (and (= massimo 3)
         (= minimo 2))))


(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        target [1 2 2]
        target-four [1 4]
        freq (sort (vals (frequencies ranks)))]
    (or (= freq target)
        (= freq target-four))))

;; couldn't wrap my head around this (tired): inspired from mikko-h solution at the end.
;; :-(

(defn straight? [hand]
  (let [high-ace (sort (map rank hand))
        low-ace (sort (replace {14 1} high-ace))
        scala-da (fn [mi] (range mi (+ mi 5)))
        low-start (apply min low-ace)
        high-start (apply min high-ace)]
    (or (= low-ace (scala-da low-start))
        (= high-ace (scala-da high-start)))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))


(defn high-card? [hand]
  true)

;; took inspiration from mikko-h solution: create a match? function
;; which outputs the checkers list filtered for elements whose first element
;; (a function) applied to hand returns false. In this way, we only keep the
;; points a hand actually totalized.
;; Then, the function values take the second element of each elements in matches, thus
;; yielding the appropriate points value for the hand. The max of this resulting
;; vector is then returned as hand best value. Not smart enough to solve this myself :-(

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        match? (fn [pair] ((first pair) hand))
        values (fn [matches] (map second matches))]
    (apply max (values (filter match? checkers)))))
