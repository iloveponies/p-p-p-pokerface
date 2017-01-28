(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14, \K 13 \Q 12, \J 11, \T 10})
  (let [[fst _] card]
  (if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
  (if (= nil snd) (str _) (str snd))))

(defn pair? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
      (contains? (set (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
      (contains? (set (vals (frequencies suits))) 5)))

(defn full-house? [hand]
    (let [ranks (sort (map rank hand))]
      (and (contains? (set (vals (frequencies ranks))) 2) (contains? (set (vals (frequencies ranks))) 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        value-freq (vals (frequencies ranks))
        pair-count (count (filter (fn [ff] (= 2 ff)) value-freq))]
    (or (= 2 pair-count) (four-of-a-kind? hand))))

(defn straight? 
  [hand]
  (let [sorted (sort (map rank hand))]
    (if (== 14 (apply max sorted)) 
      (if (= (sort (replace {14 1} (map rank hand))) (range 1 6))
        true
        (if (= (sort (map rank hand)) (range 10 15))
          true
          false))
      (= sorted (range (apply min sorted) (+ (apply min sorted) 5))))))  

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
