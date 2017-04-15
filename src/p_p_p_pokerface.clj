(ns p-p-p-pokerface)

(defn rank [card]
  (def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;Helper function for checks
(defn max-frequency [hand]
  (apply max (vals (frequencies (map rank hand)))))
;Helper function for checks
(defn hand-frequency [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (< 1 (max-frequency hand)))

(defn three-of-a-kind? [hand]
  (< 2 (max-frequency hand)))

(defn four-of-a-kind? [hand]
  (< 3 (max-frequency hand)))

(defn flush? [hand]
  (== 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (hand-frequency hand))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (hand-frequency hand))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        ace-converted (sort (replace {14 1} sorted-hand))
        is-straight? (fn [ranks] (= (range (first ranks) (+ (first ranks) 5)) ranks))]
    (or (is-straight? sorted-hand) (is-straight? ace-converted))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
