(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        faces {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (faces fst))))

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
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [sorted-hand (sort (vals (frequencies (map rank hand))))]
  (or (= [1 2 2] sorted-hand)
      (= [1 4] sorted-hand))))

(defn straight? [hand]
  (let [high-ace-hand (map rank hand)
        low-ace-hand (replace {14 1} high-ace-hand)
        is-straight? (fn [hnd]
                       (let [low (apply min hnd)]
                         (= (range low (+ low 5)) (sort hnd))))]
    (= true (some is-straight? [high-ace-hand low-ace-hand]))))

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
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
