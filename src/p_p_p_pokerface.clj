(ns p-p-p-pokerface)

(def repls {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[r -] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (repls r))))

(defn suit [card]
  (let [[_ su] card]
    (str su)))

(defn v-fr [hand]
  (vals (frequencies (mapv rank hand))))

(defn pair? [hand]
  (== (apply max (v-fr hand)) 2))
  

(defn three-of-a-kind? [hand]
  (== (apply max (v-fr hand)) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (v-fr hand)) 4))

(defn flush? [hand]
  (== (count (frequencies (mapv suit hand))) 1))


(defn full-house? [hand]
  (let [vl (v-fr hand) ]
    (and (== (count vl) 2 (apply min vl)))))

(defn two-pairs? [hand]
    (or (four-of-a-kind? hand) 
        (and (==(count (v-fr hand)) 3)
             (==(apply max (v-fr hand) ) 2))))

(defn straight? [hand]
  (let [r (sort (mapv rank hand))
	m (first r)
	r1 (sort (replace {14 1} r))
	m1 (first r1)]
    (or (= r (range m (+ m 5)))
        (= r1 (range m1 (+ m1 5)))) ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
		   [two-pairs? 2]  [three-of-a-kind? 3]
		   [straight? 4]   [flush? 5]
		   [full-house? 6] [four-of-a-kind? 7]
		   [straight-flush? 8]}]
    (apply max(map second (filter (fn [x] ((first x) hand)) checkers)))))
    
