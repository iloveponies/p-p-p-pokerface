(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind [hand n]
  (not (empty? (filter #(= true %) (map #(>= % n) (vals (frequencies (map rank hand))))))))

(defn ranks-with-ace [hand]
  (let [ranks (sort (map rank hand))]
    (if (some #(= 14 %) ranks) (cons 1 ranks) ranks)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (not (empty? (filter #(= true %) (map #(>= % 5) (vals (frequencies (map suit hand))))))))

(defn full-house? [hand]
  (true? (and
    (some #(= 2 %) (vals (frequencies (map rank hand))))
    (some #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or
    (full-house? hand)
    (four-of-a-kind? hand)
    (= 2 (count (filter #(= true %) (map #(>= % 2) (vals (frequencies (map rank hand)))))))))

(defn straight? [hand]
  (let [hand-a (ranks-with-ace hand)
        fst (first hand-a)
        snd (second hand-a)]
    (or
      (= (take 5 hand-a) (range fst (+ fst 5)))
      (= (drop 1 hand-a) (range snd (+ snd 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
