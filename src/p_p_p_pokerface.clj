(ns p-p-p-pokerface)

(def replacements 
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rk _] card
    val (cond
        (Character/isDigit rk) (Integer/valueOf (str rk))
        :else (replacements rk))]
    val))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= 
    (seq [2 3]) 
    (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (=
   (seq [1 2 2])
   (sort (vals (frequencies (map rank hand))))))
   ; four-of a kind?

(defn straight? [hand]
  (let [rephand (replace {14 1} (map rank hand))
        repminval (apply min (sort rephand))
        minval (apply min (sort (map rank hand)))]
  (or 
    (= (sort (map rank hand)) (range minval (+ minval 5)))
    (= (sort rephand) (range repminval (+ repminval 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check (fn [checker] 
                (if((first checker) hand)
                  (second checker)
                  0))]
    (apply max (map check checkers))))
