(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        reps {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (reps fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [numbers (map rank hand)
        freqs (frequencies numbers)
        freq-vals (seq (vals freqs))
        max-val (apply max freq-vals)]
    (== max-val 2)))

(defn three-of-a-kind? [hand]
  (let [numbers (map rank hand)
        freqs (frequencies numbers)
        freq-vals (seq (vals freqs))
        max-val (apply max freq-vals)]
    (== max-val 3)))


(defn four-of-a-kind? [hand]
  (let [numbers (map rank hand)
        freqs (frequencies numbers)
        freq-vals (seq (vals freqs))
        max-val (apply max freq-vals)]
    (== max-val 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)
        freq-vals (seq (vals freqs))
        max-val (apply max freq-vals)]
    (== max-val 5)))

(defn full-house? [hand]
  (let [numbers (map rank hand)
        freq-vals (sort (seq (vals (frequencies numbers))))]
    (= [2, 3] freq-vals)))

(defn two-pairs? [hand]
  (let [numbers (map rank hand)
        freq-vals (sort (seq (vals (frequencies numbers))))]
    (= [1, 2, 2] freq-vals)))


(defn test-straight? [numbers]
  (let [minv (apply min numbers)
        neededV (seq (range minv (+ minv 5)))]
    (= neededV numbers)))

(defn straight? [hand]
  (or
    (test-straight? (sort (map rank hand)))
    (test-straight? (sort (replace {14 1} (map rank hand))))
    ))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn high-card? [hand]
  (let [numbers (map rank hand)
        freq-vals (sort (seq (vals (frequencies numbers))))]
    (= [1, 1, 1, 1, 1] freq-vals)))

(def checkers #{[high-card? 0]
                [pair? 1]
                [two-pairs? 2]
                [three-of-a-kind? 3]
                [straight? 4]
                [flush? 5]
                [full-house? 6]
                [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (apply max (map second (filter (fn [x] ((first x) hand)) checkers))))



