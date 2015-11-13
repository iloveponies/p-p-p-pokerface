(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)))) 

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [fr (fn [x] (apply max (vals (frequencies (map rank x)))))]
    (if (= (fr hand) 2) true false))) 

(defn three-of-a-kind? [hand]
  (let [fr (fn [x] (apply max (vals (frequencies (map rank x)))))]
    (if (= (fr hand) 3) true false)))

(defn four-of-a-kind? [hand]
  (let [fr (fn [x] (apply max (vals (frequencies (map rank x)))))]
    (if (= (fr hand) 4) true false)))

(defn flush? [hand]
  (let [fr (fn [x] (apply max (vals (frequencies (map suit x)))))]
    (if (= (fr hand) 5) true false)))

(defn full-house? [hand]
  (let [fr (fn [x] (vals (frequencies (map rank x))))]
    (if (and (= (apply max (fr hand)) 3) (= (apply min(fr hand)) 2)) true false)))

(defn two-pairs? [hand]
  (let [fr (fn [x] (vals (frequencies (map rank x))))]
    (if (= ((frequencies (fr hand)) 2) 2) true false)))

(defn straight? [hand]
  (let [fr (fn [x] (keys (frequencies (map rank x))))]
    (let [mn (fn [y] (apply min (fr y)))]
      (let [mx (fn [z] (apply max (fr z)))]
        (if (= (mx hand) 14)
            (or (= (sort (fr hand)) [2 3 4 5 14]) (= (sort (fr hand)) [10 11 12 13 14]))
            (= (sort (fr hand)) (range (mn hand) (+ (mn hand) 5))))))))

(defn straight-flush? [hand]
  (let [fl (fn [x] (apply max (vals (frequencies (map suit x)))))]
    (and (straight? hand) (= (fl hand) 5))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}]
    (let [ck (fn [x] ((first x) hand))]
      (apply max (map second (filter ck checkers))))))
