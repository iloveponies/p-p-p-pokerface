(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
       (Integer/valueOf (str fst))
       (let [rankCharacter {
           "T" 10
           "J" 11
           "Q" 12
           "K" 13
           "A" 14 }]
           (rankCharacter (str fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn frequencies-vals [hand]
    (vals (frequencies (map rank hand))))

(defn frequencies-max [hand]
    (apply max (frequencies-vals hand)))

(defn pair? [hand]
  (< 1 (frequencies-max hand)))

(defn three-of-a-kind? [hand]
  (< 2 (frequencies-max hand)))

(defn four-of-a-kind? [hand]
  (< 3 (frequencies-max hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [[fst snd] (sort (frequencies-vals hand))]
    (= [2 3] [fst snd])))

(defn two-pairs? [hand]
  (let [sorted (sort (frequencies-vals hand))]
    (or (= [1 4] sorted) (= [1 2 2] sorted))))

(defn straight? [hand]
  (let [ace-up (sort (map rank hand))
        ace-down (sort (replace {14 1} ace-up))
        compare-sorted-aces (fn [sorted-hand]
            (let [hand-min (apply min sorted-hand)]
                (= (range hand-min (+ hand-min 5)) sorted-hand)))]
      (or (compare-sorted-aces ace-up) (compare-sorted-aces ace-down))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
    true)

(defn value [hand]
    (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
          (apply max (map second (filter (fn [item] ((first item) hand)) checkers)))))
