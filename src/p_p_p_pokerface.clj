(ns p-p-p-pokerface)

(defn rank [card]
  (let[rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}
       [card-rank _] card]
    (if(Character/isDigit card-rank) (Integer/valueOf (str card-rank)) (get rank-map card-rank))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
   (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn two-pairs? [hand]
  (let [struc (sort (vals (frequencies (map rank hand))))]
    (or (= struc [1 2 2]) (= struc [1 4]))))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn straight? [hand]
 (let [ranks (sort (map rank hand))
       min-rank (first ranks)
       straight (range min-rank (+ min-rank 5))
       replace-ace (sort (replace {14 1} ranks))
       min-rank2 (first replace-ace)
       straight2 (range min-rank2 (+ min-rank2 5))]
 (or (= ranks straight)
     (= replace-ace straight2))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]
        has-value? (fn [hand val] ((get checkers val) hand))
        try-value (fn [x] (has-value? hand x))]
     (apply max (filter try-value (range 0 9)))))