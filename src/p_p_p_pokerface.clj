(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
  (= (seq [2 3]) (sort freqs))))

(defn two-pairs? [hand]
  (let [sortfreqs (sort (vals (frequencies (map rank hand))))]
  (or (= (seq [1 2 2]) sortfreqs) (= (seq [2 3]) sortfreqs) (= (seq [1 4]) sortfreqs))))

(defn straight? [hand]
  (let [sortrank (sort (map rank hand))]
    (def judge (fn [sortrank] (= (range (first sortrank) (+ 5 (first sortrank))) sortrank)))
    (if (judge sortrank) true (judge (sort (replace {14 1} sortrank))))
    ))

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
        matched (filter (fn [checker] ((first checker) hand)) checkers)]

    (apply max (map second matched))))
