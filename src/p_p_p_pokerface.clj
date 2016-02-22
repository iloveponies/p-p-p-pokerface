(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[fst snd] card]
  (if (Character/isDigit fst) (Integer/valueOf (str fst))
    (get replacements fst))))


(defn suit [card]
  (let [[fst snd] card]
  (str snd)))

(defn pair? [hand]
  (let [comb (frequencies (map rank hand))
        match (vals comb)]
    (and (== (apply max match) 2) (== (count match) 4))
     ))

(defn three-of-a-kind? [hand]
  (let [comb (frequencies (map rank hand))
        match (vals comb)]
    (and (== (apply max match) 3) (== (count match) 3))
     ))

(defn four-of-a-kind? [hand]
    (== (apply max (vals (frequencies (map rank hand)))) 4)
     )

(defn flush? [hand]
 (== (count (keys (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (let [comb (frequencies (map rank hand))
        match (vals comb)]
    (and (== (apply max match) 3) (== (count match) 2))
     ))

(defn two-pairs? [hand]
  (let [comb (frequencies (map rank hand))
        match (vals comb)]
    (and (== (apply max match) 2) (== (count match) 3))
     ))

(defn straight? [hand]
   (let [high-values (sort (map rank hand))
        low-values (sort (replace {14 1} high-values))
        card-combinations (frequencies high-values)
        high-start (first high-values)
        high-straight (range high-start (+ high-start 5))
        low-start (first low-values)
        low-straight (range low-start (+ low-start 5))]
    (or (= high-values high-straight) (= low-values low-straight))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
 (cond
  (straight-flush? hand) 8
  (four-of-a-kind? hand) 7
  (full-house? hand) 6
  (flush? hand) 5
  (straight? hand) 4
  (three-of-a-kind? hand) 3
  (two-pairs? hand) 2
  (pair? hand) 1
  :else 0))
