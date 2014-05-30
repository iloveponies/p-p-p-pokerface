(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= (apply max freq) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= (apply max freq) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= (apply max freq) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (vals (frequencies suits))]
    (= (first freq) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= (sort freq) '(2 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= (sort freq) '(1 2 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        lowest (first (sort ranks))
        ranks-low-ace (replace {14 1} ranks)]
    (or (= (map (fn [x] (- x lowest)) (sort ranks))
           (range 5))
        (= (map dec (sort ranks-low-ace))
           (range 5)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        values (filter #((first %) hand) checkers)]
    (apply max (map second values))))
