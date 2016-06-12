(ns p-p-p-pokerface)

(defn suit [card]
  (str (second card)))

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn hand-rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (not (= (some #{2} (hand-rank-frequencies hand)) nil)))

(defn three-of-a-kind? [hand]
  (not (= (some #{3} (hand-rank-frequencies hand)) nil)))

(defn four-of-a-kind? [hand]
  (let [rank-freqs (hand-rank-frequencies hand)]
    (or (= rank-freqs '(1 4))
        (= rank-freqs '(4 1)))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [rank-freqs (hand-rank-frequencies hand)] 
    (or (= rank-freqs '(2 3))
        (= rank-freqs '(3 2)))))

(defn two-pairs? [hand]
  (= 2 (get (frequencies (hand-rank-frequencies hand)) 2)))

(defn straight? [hand]
  (let [low-ace-straight (sort (replace {14 1} (map rank hand)))
        other-straight (sort (map rank hand))]
    (or (= low-ace-straight (range (first low-ace-straight) (+ (first low-ace-straight) 5)))
        (= other-straight (range (first other-straight) (+ (first other-straight) 5))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (let [checkers [[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]]]
  (apply max (filter (fn [x] x) (map (fn [matcher]
                                       (let [f (first matcher)]
                                         (if (f hand) (second matcher)))) checkers)))))

