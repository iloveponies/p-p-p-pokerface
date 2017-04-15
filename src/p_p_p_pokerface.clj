(ns p-p-p-pokerface)

(def replacements
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[value _] card]
    (if (Character/isDigit value) (Integer/valueOf (str value)) (get replacements value))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (and (contains? (set values) 2) (not (= (count (set values)) (count values))))))

(defn three-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (and (contains? (set values) 3))))

(defn four-of-a-kind? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (and (contains? (set values) 4))))

(defn flush? [hand]
  (let [values (sort (map rank hand))
        suits (set (map suit hand))]
    (and (apply < values) (= (count suits) 1))))

(defn full-house? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
    (and (= (first freq) 2) (= (second freq) 3))))

(defn two-pairs? [hand]
  (let [frqncy (sort (vals (frequencies (map rank hand))))
        two (filter (fn [x] (= x 2)) frqncy)
        twocount (count two)]
    (or (= twocount 2) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        max-value (apply max values)
        min-value (apply min values)]
    (if (and (= min-value 2) (= max-value 14))
      (let [real-min 1
            real-max (nth values 3)]
        (= (sort (replace {14 1} values)) (range real-min (+ real-max 1))))
      (= values (range min-value (+ max-value 1))))))

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



