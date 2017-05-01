(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 2 (apply max (vals freq)))))

(defn three-of-a-kind? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 3 (apply max (vals freq)))))

(defn four-of-a-kind? [hand]
  (let [freq (frequencies (map rank hand))]
    (= 4 (apply max (vals freq)))))

(defn flush? [hand]
  (let [freq (frequencies (map suit hand))]
    (= 5 (apply max (vals freq)))))

(defn full-house? [hand]
  (let [freq (frequencies (map rank hand))
        freq-vals (sort (vals freq))]
    (= (range 2 4) freq-vals)))

(defn two-pairs? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
     (or (= freq [1 2 2]) (= 4 (apply max freq)))))

(defn straight? [hand]
  (let [r (map rank hand)
        ranks (if (< (apply min r) 10)
                (replace {14 1} r)
                r)
        min-rank (apply min ranks)
        max-rank (apply max ranks)]
    (= (sort ranks) (range min-rank (+ 1 max-rank)))))

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
        filtered (filter (fn [x] ((first x) hand)) checkers)
        values (map second filtered)]
          (apply max values)))

