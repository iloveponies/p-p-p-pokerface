(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
   (if
    (Character/isDigit rnk)
     (Integer/valueOf (str rnk))
     (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))

(defn suit [card]
  (let [
        [_ st] card]
    (str st)))

(defn pair? [hand]
  (== 2 
   (apply max
    (vals
     (frequencies
      (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 
   (apply max
    (vals
     (frequencies
      (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 
   (apply max
    (vals
     (frequencies
      (map rank hand))))))

(defn flush? [hand]
  (== 5
   (apply max
    (vals
     (frequencies
      (map suit hand))))))

(defn full-house? [hand]
  (= [2 3]
   (sort
    (vals
     (frequencies 
      (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2]
   (sort
    (vals
     (frequencies 
      (map rank hand))))))

(defn straight? [hand]
  (let [A-14 (sort (map rank hand))
        A-1 (sort (replace {14 1} A-14))
        A-14-min (apply min A-14)
        A-1-min (apply min A-1)
        A-14-straight  (range A-14-min (+ 5 A-14-min))
        A-1-straight (range A-1-min (+ 5 A-1-min))]
    (or (= A-14 A-14-straight) 
        (= A-1 A-1-straight))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (apply max (filter (fn [n] (hand-has-value? hand n)) (range 0 9))))