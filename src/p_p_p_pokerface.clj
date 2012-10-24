(ns p-p-p-pokerface)

(defn rank
   "Palauttaa kortin arvon"
   [card]
   (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
   (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (replacements rank))))

(defn suit
  "Palauttaa kortin maan"
  [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair?
   "Tutkii onko kädessä pareja"
   [hand]
   (<= 1 (count 
          (filter (fn [x] (= x 2))
          (vals (frequencies (map rank hand)))))) )
        
(defn four-of-a-kind?
  "Tutkii onko kädessä neljä samaa lukua"
  [hand]
  (let [values (vals (frequencies (map rank hand)))
        fours (filter (fn [x] (>= x 4)) values)]
    (<= 1 (count fours))))

(defn two-pairs?
  "Tutkii onko kädessä kaksi paria tai neloset"
  [hand]
  (let [values (vals (frequencies (map rank hand)))
        pairs (filter (fn [x] (= x 2)) values)]
     (or (= 2 (count pairs)) (four-of-a-kind? hand))))

(defn three-of-a-kind?
  "Tutkii onko kädessä kolme samaa lukua"
  [hand]
  (let [values (vals (frequencies (map rank hand)))
        threes (filter (fn [x] (>= x 3)) values)]
       (<= 1 (count threes)) ))

(defn straight? [hand]
  (let [values (sort (map rank hand))
       [eka _ _ _ vika] values
       straight (range eka (+ eka 5))]
        (or (= values '(2 3 4 5 14)) (= values straight))))

(defn flush?
  "Tutkii ovatko kaikki kortit samaa maata"
  [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house?
  "Tutkii onko kädessä pari ja kolmoset"
  [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn straight-flush?
  "Tutkii onko kädssä vain yhtä maata"
  [hand]
  (and (flush? hand) (straight? hand)))

(defn value
  "Laskee kädelle arvon - en nyt onnistunut sisäistämään sievennystekniikoita toistaiseksi"
  [hand]
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


