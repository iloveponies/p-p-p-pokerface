(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (ranks rank))))

(defn suit [[_ suit]]
  (str suit))

(defn n-of-a-kind [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (>= (n-of-a-kind hand) 2))

(defn three-of-a-kind? [hand]
  (>= (n-of-a-kind hand) 3))

(defn four-of-a-kind? [hand]
  (>= (n-of-a-kind hand) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [frqs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] frqs)
        (= [1 4]   frqs))))

(defn straight? [hand]
  (let [a-seq (map rank hand)]
    (let [a-seq (sort (if (not= (first a-seq) 10)
                        (replace {14 1} a-seq)
                        a-seq))]
       (= (range (first a-seq) (+ (last a-seq) 1)) a-seq))))
;      (= [0 1 2 3 4] (map (fn [v] (- v (first a-seq))) a-seq)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
  ((get checkers value) hand)))

(defn hand-has-type? [hand [checker value]]
  (and (checker hand) (hand-has-value? hand value)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        ; Get all checker types that this hand has.
        valid-checkers (filter (fn [checker] (hand-has-type? hand checker)) checkers)]
    ; Get the checker with the biggest value.
    (apply max (map (fn [checker] (get checker 1)) valid-checkers))))
