(ns p-p-p-pokerface)

(defn rank [card]
  (let [m {\T 10, \J 11, \Q 12, \K 13, \A 14}
       [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get m r))))


(defn suit [card]
  (let [[_ s] card] (str s)))

(defn pair? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
  (and (== 2 (apply max freq)))))

(defn three-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
  (and (== 3 (apply max freq)))))

(defn four-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
  (and (== 4 (apply max freq)))))

(defn flush? [hand]
  (let [freq (vals (frequencies (map suit hand)))]
  (and (== 5 (apply max freq)))))

(defn full-house? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
  (and (== 2 (apply min freq))
       (== 3 (apply max freq)))))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
  (and (== 2 (apply max freq))
       (== 2 (get (frequencies freq) 2)))))

(defn straight? [hand]
  (let [check_ranks
        (fn [ranks]
            (and (== 4 (- (apply max ranks)
                          (apply min ranks)))
                 (== 5 (count ranks))))
        ranks (set (map rank hand))
        ranks2 (set (replace {14 1} (map rank hand)))]
    (or (check_ranks ranks)
        (check_ranks ranks2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (let [matching-hands (filter (fn [c] (let [[checker _]  c] (checker hand))) checkers)
        matching-values (map (fn [mh] (let [[_ value] mh] value)) matching-hands)]
    (apply max matching-values))))
