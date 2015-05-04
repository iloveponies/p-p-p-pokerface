(ns p-p-p-pokerface)

(def values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk) (Integer/valueOf (str rnk)) (values rnk))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (= (get (frequencies freq) 2) 1)))

(defn three-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (= (get (frequencies freq) 3) 1)))

(defn four-of-a-kind? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (= (get (frequencies freq) 4) 1)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
 (let [freq (vals (frequencies (map rank hand)))]
  (= [2 3] (sort freq))))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
   (or (four-of-a-kind? hand) (= (get (frequencies freq) 2) 2))))

(defn straight? [hand]
 (let [temp (map rank hand)
       low (replace {14 1} temp)
       [x y z d c] (sort temp)]
  (or (= (range x (+ c 1)) (sort temp))
      (= (range 1 6) (sort low)))))

(defn straight-flush? [hand]
 (and (straight? hand)
      (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (map second (filter (fn [checker] ((first checker) hand)) checkers))]
 (apply max values)))
