(ns p-p-p-pokerface)

(def rank-replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-replacements r))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn n-kind? [n hand]
  (<= n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-kind? 4 hand))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [values (set (vals (frequencies (map rank hand))))]
    (and (contains? values 3) (contains? values 2))))


(defn two-pairs? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] values)
        (full-house? hand)
        (four-of-a-kind? hand))))

; Check for a straight sequence using the tools we know
(defn straight-seq-helper [values]
 (let [minv (apply min values)
       vs (map (fn [x] (- x minv)) values)]
   (= vs (range 0 5))))

(defn straight-seq? [values]
  (or (straight-seq-helper (sort values))
      (straight-seq-helper (sort (replace {14 1} values)))))

; five cards in sequence, at least two different suits
(defn straight? [hand]
  (let [values (sort (map rank hand))
        suits (set (map suit hand))
        st (count suits)]
    (and (> st 1) (straight-seq? values))))

; five cards in sequence, all of the same suit
(defn straight-flush? [hand]
  (let [values (sort (map rank hand))
        suits (set (map suit hand))]
    (and (flush? hand) (straight-seq? values))))

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
