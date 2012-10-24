(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        r {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (contains? r fst)
      (get r fst)
      (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[fst snd] card]
  (str snd)))

(defn pair? [hand]
   (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
 (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (let [s (vals (frequencies (map suit hand)))]
  (= (str "(5)") (str s))))

(defn full-house? [hand]
  (let [[x y] (sort (vals (frequencies (map rank hand))))]
   (and (== 2 x) (== 3 y))))

(defn two-pairs? [hand]
  (let [[x y] (sort (vals (frequencies (map rank hand))))]
   (< 1 y)))

(defn straight? [hand]
  )

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)