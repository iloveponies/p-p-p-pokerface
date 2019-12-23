(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\A 14, \T 10, \J 11, \Q 12, \K 13})
  (let [[fst snd] card]
    (if(Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (if(> (apply max (vals (frequencies (mapv rank hand)))) 1) true false))

(defn three-of-a-kind? [hand]
  (if(> (apply max (vals (frequencies (mapv rank hand)))) 2) true false))

(defn four-of-a-kind? [hand]
  (if(> (apply max (vals (frequencies (mapv rank hand)))) 3) true false))

(defn flush? [hand]
  (if(> (apply max (vals (frequencies (mapv suit hand)))) 4) true false))

(defn full-house? [hand]
  (if(= (sort (vals (frequencies (mapv rank hand)))) (range 2 4)) true false))

(defn two-pairs? [hand]
  (if(or (= (sort (vals (frequencies (mapv rank hand)))) (seq [1 2 2])) (four-of-a-kind? hand)) true false))

(defn straight? [hand]
  (defn rankstr [card]
    (let [[fst snd] card]
      (if(Character/isDigit fst)
        (Integer/valueOf (str fst))
        (str fst))))
  (let [ranks (mapv rankstr hand)
        sortranks1 (sort (replace {"A" 1, "T" 10, "J" 11, "Q" 12, "K" 13 } ranks))
        sortranks14 (sort (replace {"A" 14, "T" 10, "J" 11, "Q" 12, "K" 13 } ranks))]
    (if(or (= sortranks1 (range (apply min sortranks1) (+ (apply min sortranks1) 5))) (= sortranks14 (range (apply min sortranks14) (+ (apply min sortranks14) 5)))) true false)))

(defn straight-flush? [hand]
  (if(and (flush? hand) (straight? hand)) true false))

(defn value [hand]
  (defn high-card? [hand]
    true)
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [fname] ((first fname) hand)) checkers)))))
