(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [ch (get card 0)]
  (cond (Character/isDigit ch) (Integer/valueOf (str ch))
         :else (Integer/valueOf (replacements ch)))))

(defn suit [card]
  (str (get card 1)))

(defn values-of-a-hand [hand]
  (vals (frequencies (map rank hand))))

(defn max-amount [hand]
  (apply max (values-of-a-hand hand)))

(defn pair? [hand]
  (< 1 (max-amount hand)))

(defn three-of-a-kind? [hand]
  (< 2 (max-amount hand)))

(defn four-of-a-kind? [hand]
  (< 3 (max-amount hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [sorted-vals (sort (values-of-a-hand hand))]
    (and (= 2 (first sorted-vals))
         (= 3 (first (rest sorted-vals))))))

(defn two-pairs? [hand]
  (let [sorted-vals (sort (values-of-a-hand hand))
        firstv (first sorted-vals)
        secondv (first (rest sorted-vals))
        thirdv (first (rest (rest sorted-vals)))]
    (or (= 2 firstv secondv) (= 2 secondv thirdv)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        firstv (first values)
        lastv (+ 4 firstv)
        test-values (range firstv (+ 1 lastv))]
    (or (= values test-values)
        (= [2 3 4 5 14] values))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else                   0))
