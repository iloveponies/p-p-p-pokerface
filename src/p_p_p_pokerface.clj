(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12,
                     \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [pair-rank
        (fn [card] (rank card))]
    (if (>= (apply max (vals (frequencies
                               (mapv pair-rank hand)))) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
    (let [pair-rank
        (fn [card] (rank card))]
    (if (>= (apply max (vals (frequencies
                               (mapv pair-rank hand)))) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
    (let [pair-rank
        (fn [card] (rank card))]
    (if (>= (apply max (vals (frequencies
                               (mapv pair-rank hand)))) 4)
      true
      false)))

(defn flush? [hand]
  (let [pair-suit
        (fn [card] (suit card))]
    (if (= (count hand) (apply max (vals (frequencies
                                           (mapv pair-suit hand)))))
      true
      false)))

(defn full-house? [hand]
  (let [rank-seq
        (fn [card] (rank card))]
    (= [2 3](sort (vals (frequencies (mapv rank-seq hand)))))))

(defn two-pairs? [hand]
  (let [rank-seq
        (fn [card] (rank card))]
    (if (or (= [1 2 2] (sort (vals (frequencies (mapv rank-seq hand)))))
            (= [1 4] (sort (vals (frequencies (mapv rank-seq hand))))))
      true
      false)))

(defn straight? [hand]
  (let [get-rank
        (fn [card] (rank card))
        hand-sorted
        (fn [hand] (sort (mapv get-rank hand)))
        get-min
        (fn  [hand] (apply min (hand-sorted hand)))
        get-max
        (fn [hand] (apply max (hand-sorted hand)))
        comp-hand
        (fn [min-val max-val] (range min-val (+ 1 max-val)))
        ace-as-1
        (fn [hand] (sort (replace {14 1} (hand-sorted hand))))
        get-min-ace
        (fn  [hand] (apply min (ace-as-1 hand)))
        get-max-ace
        (fn [hand] (apply max (ace-as-1 hand)))]
    (if (= (comp-hand (get-min-ace hand) (get-max-ace hand)) (ace-as-1 hand))
      true
      (if (= (comp-hand (get-min hand) (get-max hand)) (hand-sorted hand))
        true
        false))))

(defn straight-flush? [hand]
  (if (straight? hand)
    (if (flush? hand)
      true
      false)
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]    [pair? 1]
                   [two-pairs? 2]    [three-of-a-kind? 3]
                   [straight? 4]     [flush? 5]
                   [full-house? 6]   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        helper-f
          (fn [pari] ((first pari) hand))]
   (apply max (map second (filter helper-f checkers)))))
