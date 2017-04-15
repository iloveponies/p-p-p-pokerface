(ns p-p-p-pokerface)

(defn rank [card]
  (let [[x _] card y {\T 10, \J 11, \Q 12, \K 13, \A 14} ]
  (if (Character/isDigit x) (Integer/valueOf (str x)) (y x))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [x (mapv rank hand) y (first x)]
  (or (= (sort x) (range y (+ y 5))) (= (sort x) [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house?  hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))
