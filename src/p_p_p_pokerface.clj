(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (sort (vals (frequencies ranks))) (range 2 4))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or (= (sort (vals (frequencies ranks))) '(1 2 2))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        rank-seq (sort ranks)]
    (and (apply < rank-seq)
         (or (= (/ (+ (first rank-seq) (last rank-seq)) 2) (nth rank-seq 2))
             (= (/ (+ (first rank-seq) (last rank-seq)) 2) 8)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]     [pair? 1]
                   [two-pairs? 2]     [three-of-a-kind? 3]
                   [straight? 4]      [flush? 5]
                   [full-house? 6]    [four-of-a-kind? 7]
                   [straight-flush? 8]}
         apply-fn (fn [pair] ((first pair) hand))]
    (apply max (map second (filter apply-fn checkers)))))
