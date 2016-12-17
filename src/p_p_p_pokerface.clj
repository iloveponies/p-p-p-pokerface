(ns p-p-p-pokerface)

(defn rank [card]
  (let [higher-ranks {\T, 10
                      \J, 11
                      \Q, 12
                      \K, 13
                      \A, 14}
        [rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get higher-ranks rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [freq-map (frequencies (mapv rank hand))
        freq (vals freq-map)
        contains-2 (some #{2} freq)]
    (= contains-2 2)))

(defn three-of-a-kind? [hand]
  (let [freq-map (frequencies (mapv rank hand))
        freq (vals freq-map)
        contains-3 (some #{3} freq)]
    (= contains-3 3)))

(defn four-of-a-kind? [hand]
  (let [freq-map (frequencies (mapv rank hand))
        freq (vals freq-map)
        contains-4 (some #{4} freq)]
    (= contains-4 4)))

(defn flush? [hand]
  (let [freq-map (frequencies (mapv suit hand))
        freq (vals freq-map)]
  (= (apply max freq) 5)))

(defn full-house? [hand]
  (let [freq-map (frequencies (mapv rank hand))
        freq (vals freq-map)
        sort-freq (sort freq)]
    (= sort-freq (seq [2 3]))))


(defn two-pairs? [hand]
  (let [freq-map (frequencies (mapv rank hand))
        freq (vals freq-map)
        eq-2 (fn [x] (= x 2))
        num2 (filter eq-2 freq)]
    (or (= 2 (count num2))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [vals (map rank hand)
        sort-vals (sort vals)
        a-low-vals (replace {14 1} sort-vals)
        s? (fn [nums] (and (apply < nums)
                           (or (= (- (get (vec nums) 4) (get (vec nums) 0)) 4)
                               (= (- (get (vec nums) 4) 1) 13))) )]
    (or (s? sort-vals)
        (s? a-low-vals))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-function (fn [val-pair] ((first val-pair) hand))
        matching (filter check-function checkers)
        matching-vals (map second matching)]
    (apply max matching-vals)))
