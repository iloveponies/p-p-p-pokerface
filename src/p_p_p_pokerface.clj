(ns p-p-p-pokerface)

(def face {\T 10,
           \J 11,
           \Q 12,
           \K 13,
           \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit (char r))
      (Integer/valueOf (str r))
      (face r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (map rank hand)))) [1 2 2])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-low-ranks (replace {14 1} ranks)
        low (apply min ranks)
        straight (range low (+ low 5))
        ace-low-low (apply min ace-low-ranks)
        ace-low-straight (range ace-low-low (+ ace-low-low 5))]
    (or (= straight (sort ranks))
        (= ace-low-straight (sort ace-low-ranks)))))

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
        valid (filter #((first %) hand) checkers)
        scores (map second valid)
        best (apply max scores)]
    best))
