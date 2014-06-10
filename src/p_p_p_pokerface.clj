(ns p-p-p-pokerface)

(defn rank [card]
  (let [[[strval _] lookup] [card {\T 10 \J 11 \Q 12 \K 13 \A 14}]]
    (cond
     (Character/isDigit strval) (Integer/valueOf (str strval))
     :else (lookup strval))))

(defn suit [card]
  (let [[_ snd] card] 
    (str snd)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn max-matches [hand i]
  (= (apply max (rank-frequencies hand)) i))

(defn pair? [hand]
  (max-matches hand 2))

(defn three-of-a-kind? [hand]
  (max-matches hand 3))

(defn four-of-a-kind? [hand]
  (max-matches hand 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freqs (sort (rank-frequencies hand))]
    (and
     (= (first freqs) 2)
     (= (last freqs) 3))))

(defn two-pairs? [hand]
  (== 2 (count (filter (fn [x] (== x 2)) (rank-frequencies hand)))))

(defn in-a-row? [card1 sorted-hand] 
  (= (range card1 (+ card1 5)) sorted-hand))

(defn straight? [hand]
  (let [ordered-hand (sort (map rank hand))]
    (let [[start high-ace low-ace] [(first ordered-hand) 14 1]]
      (cond
       (and (= start 2) 
            (= (last ordered-hand) high-ace)) (in-a-row? 
                                               low-ace (sort (replace {high-ace low-ace} ordered-hand)))
       :else (in-a-row? start ordered-hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(def hand-types
  [high-card? 
   pair? 
   two-pairs? 
   three-of-a-kind? 
   straight? 
   flush? 
   full-house? 
   four-of-a-kind? 
   straight-flush?])

(def hand-type-val
  (zipmap hand-types (range (count hand-types))))

(defn value [hand]
  (apply max (map (fn [[k v]] (if (k hand) v 0)) hand-type-val)))
