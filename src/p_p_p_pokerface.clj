(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn hand->freqset [hand]
  (set (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (contains? (hand->freqset hand) 2))

(defn high-card? [hand]
  (not (pair? hand)))

(defn three-of-a-kind? [hand]
  (contains? (hand->freqset hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (hand->freqset hand) 4))

(defn flush? [hand]
  (let [suits (mapv suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (vec (vals (frequencies (map rank hand))))]
    (== (count (filter #{2} freqs)) 2)))

(defn straight? [hand]
  (let [ranks (sort (vec (mapv rank hand)))
        ranksa (sort (replace [0 14 2 3 4 5 6 7 8 9 10 11 12 13 1] ranks))]
    (or (= ranks (range (first ranks) (+ (first ranks) 5)))
        (= ranksa (range (first ranksa) (+ (first ranksa) 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn testers [hand]
  (let [check [pair? 0]]
    (if ((first check) hand) 0 1)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checky (fn [check] (if ((first check) hand) (second check) 0))
        hands (map checky checkers)]
    (apply max hands)))
