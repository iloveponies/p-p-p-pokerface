(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card
        r-num {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get r-num r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn- hand-to-vals [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
    (= [2 1 1 1] (reverse (sort (hand-to-vals hand)))))

(defn three-of-a-kind? [hand]
  (= [3 1 1] (hand-to-vals hand)))

(defn four-of-a-kind? [hand]
  (= [4 1] (hand-to-vals hand)))

(defn flush? [hand]
  (let [s (suit (first hand))]
    (every? #(= s (suit %)) (rest hand))))

(defn full-house? [hand]
  (= [3 2] (hand-to-vals hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= [2 2 1] (hand-to-vals hand))))

(defn straight? [hand]
  (let [ordered (sort (map rank hand))
        ace-high (range 10 15)
        five-high [2 3 4 5 14]]
    (or (= ace-high ordered)
        (= five-high ordered)
        (= ordered (range (first ordered) (+ 1 (last ordered)))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        bar (fn [[p _]] (p hand))
        foo (filter bar checkers)]
    (apply max (map #(second %) foo))))
