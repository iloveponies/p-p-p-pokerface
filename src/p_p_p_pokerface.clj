(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [1 1 1 2])))

(defn three-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [1 1 3])))

(defn four-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [1 4])))

(defn flush? [hand]
  (= (vals (frequencies (map suit hand)))
     (seq [5])))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [2 3])))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [1 2 2])))

(defn straight-norepl? [sorted-hand-values]
  (let [firstval (first sorted-hand-values)]
    (= (map (fn [x] (- x firstval)) sorted-hand-values)
       (range 5))))

(defn straight? [hand]
  (let [big-ace-hand (sort (map rank hand))
        small-ace-hand (sort (replace {14 1} (map rank hand)))]
    (cond
     (straight-norepl? big-ace-hand) true
     (straight-norepl? small-ace-hand) true
     :else false)
  ))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   (high-card? hand)       0
  ))
