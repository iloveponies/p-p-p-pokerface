(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        rnkmap {\T 10 \J 11 \Q 12 \K 13 \A 14}]
  (if (Character/isDigit rnk)
    (Integer/valueOf (str rnk))
    (rnkmap rnk))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-matched-rank? [hand number]
  (>= (apply max (vals (frequencies (map rank hand)))) number))

(defn pair? [hand]
  (n-matched-rank? hand 2))

(defn three-of-a-kind? [hand]
  (n-matched-rank? hand 3))

(defn four-of-a-kind? [hand]
  (n-matched-rank? hand 4))

(defn flush? [hand]
  (= 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= #{2 3} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (= '(1 2 2) (sort (vals (frequencies (map rank hand)))))
      (full-house? hand)
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [lowest (apply min (map rank hand))
        test-straight (range lowest (+ lowest 5))
        sorted (sort (map rank hand))]
    (or (= sorted test-straight)
        (and (= (last sorted) (rank "A?")) ; includes ace
             (= (butlast sorted) '(2 3 4 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [scores #{
                [pair? 1]
                [two-pairs? 2]
                [three-of-a-kind? 3]
                [straight? 4]
                [flush? 5]
                [full-house? 6]
                [four-of-a-kind? 7]
                [straight-flush? 8]}]
     (last (sort (map (fn [score] (if ((first score) hand) (second score))) scores)))))

(defn high-card? [card]
  true)
