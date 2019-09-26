(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (>= (apply max values) 2)))

(defn three-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (>= (apply max values) 3)))

(defn four-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (>= (apply max values) 4)))

(defn flush? [hand]
  (let [values (vals (frequencies (map suit hand)))]
    (>= (apply max values) 5)))

(defn full-house? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (= (sort values) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (= (sort values) (seq [1 2 2]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        new-ranks (replace {14 1} ranks)
        min-rank (fn [r] (apply min r))]
    (or (= (sort ranks) (range (min-rank ranks) (+ (min-rank ranks) 5)))
        (= (sort new-ranks) (range (min-rank new-ranks) (+ (min-rank new-ranks) 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-hand (fn [checker] ((first checker) hand))]
    (apply max (map second (filter check-hand checkers)))))
