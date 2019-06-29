(ns p-p-p-pokerface)

(defn replaceable-letter-rank [card replacements]
  (let [[rank-char _] card]
    (cond
     (Character/isDigit rank-char) (Integer/valueOf (str rank-char))
     :else (get replacements rank-char))))

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (replaceable-letter-rank card replacements)))

(defn low-ace-rank [card]
  (let [replacements {\A 1 \T 10 \J 11 \Q 12 \K 13}]
    (replaceable-letter-rank card replacements)))

(defn suit [card]
  (str (get card 1)))

(defn rank-count [hand]
  (vals (frequencies (map rank hand))))

(defn has-num-same-cards? [numcards hand]
  (== (apply max (rank-count hand)) numcards))

(defn pair? [hand]
  (has-num-same-cards? 2 hand))

(defn three-of-a-kind? [hand]
  (has-num-same-cards? 3 hand))

(defn four-of-a-kind? [hand]
  (has-num-same-cards? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [rank-list (sort (rank-count hand))]
    (= (range 2 4) rank-list)))

(defn two-pairs? [hand]
  (let [rank-list (sort (rank-count hand))]
    (or
     (four-of-a-kind? hand)
     (= [1 2 2] (sort (rank-count hand))))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        low-ace-sorted-ranks (sort (map low-ace-rank hand))
        min-max-rank-range (fn [ranks]
                             (range
                              (apply min ranks)
                              (+ (apply max ranks) 1)))]
    (or
     (= (min-max-rank-range sorted-ranks) sorted-ranks)
     (= (min-max-rank-range low-ace-sorted-ranks) low-ace-sorted-ranks))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        curr-pred (fn [a-val] ((first a-val) hand))
        values (fn [filtered-checkers] (map second filtered-checkers))
        max-value (fn [values] (apply max values))]
    (max-value (values (filter curr-pred checkers)))))
