(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14 })
      (if (Character/isDigit fst) (Integer/valueOf (str fst))
        (replacements fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 1) true
    false))

(defn three-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 2) true
    false))

(defn four-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 3) true
    false))

(defn flush? [hand]
  (if (= (apply max (vals (frequencies (map suit hand)))) 5) true
    false))

(defn full-house? [hand]
  (if (= (sort (vals (frequencies (map rank hand)))) [2 3]) true
    false))

(defn two-pairs? [hand]
  (if (= (sort (vals (frequencies (map rank hand)))) [1 2 2]) true
    false))

(defn straight? [hand]
  (let [card-list (map rank hand) another-card-list (replace {14 1} (map rank hand))]
    (or (= (range (apply min card-list) (inc (apply max card-list))) (sort card-list))
      (= (range (apply min another-card-list) (inc (apply max another-card-list))) (sort another-card-list)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4]
    [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]
    (let [check-hand (fn [x] (let [[matcher value] x]
      (if (matcher hand) value 0)))]
        (apply max (map check-hand checkers))
  )))
