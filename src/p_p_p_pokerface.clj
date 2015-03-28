(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-vals {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (get rank-vals rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))
        maxcards (apply max freqs)]
    (>= maxcards 2)))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))
        maxcards (apply max freqs)]
    (>= maxcards 3)))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))
        maxcards (apply max freqs)]
    (>= maxcards 4)))

(defn flush? [hand]
  (let [suits (vals (frequencies (map suit hand)))
        maxsuit (apply max suits)]
    (= maxsuit 5)))

(defn full-house? [hand]
  (let [ranks (vals (frequencies (map rank hand)))
        maxrank (apply max ranks)
        minrank (apply min ranks)]
    (and
     (= maxrank 3)
     (= minrank 2))))

(defn two-pairs? [hand]
  (let [ranks (vals (frequencies (mapv rank hand)))
        maxrank (apply max ranks)
        minrank (apply min ranks)]
    (or 
     (and
      (= maxrank 2)
      (= minrank 1)
      (= (count ranks) 3))
     (= maxrank 4))))

(defn straight? [hand]
  (let [ranks1 (map rank hand)
        ranks2 (replace {14 1} ranks1)
        sorted-hand1 (sort ranks1)
        sorted-hand2 (sort ranks2)
        straight-hand1 (range (apply min sorted-hand1) (inc (apply max sorted-hand1)))
        straight-hand2 (range (apply min sorted-hand2) (inc (apply max sorted-hand2)))]
    (or (= sorted-hand1 straight-hand1)
        (= sorted-hand2 straight-hand2))))
  

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))
