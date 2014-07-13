(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value _] card]
  (if (Character/isDigit value)
  (Integer/valueOf (str value))
  ({\T 10 \J 11 \Q 12 \K 13 \A 14} value))))

(defn suit [card]
  (let [[_ color] card]
  (str color)))

(defn pair? [hand]
  (let [wart (map rank hand)]
  (if (= 2 (apply max (vals (frequencies wart))))
  true
  false)))

(defn three-of-a-kind? [hand]
  (let [wart (map rank hand)]
  (if (= 3 (apply max (vals (frequencies wart))))
  true
  false)))

(defn four-of-a-kind? [hand]
  (let [wart (map rank hand)]
  (if (= 4 (apply max (vals (frequencies wart))))
  true
  false))) 

(defn flush? [hand]
  (let [wart (map suit hand)]
  (if (= 5 (apply max (vals (frequencies wart))))
  true
  false)))

(defn full-house? [hand]
  (let [wart (map rank hand)]
  (if (and (= 3 (apply max (vals (frequencies wart)))) (= 2 (apply min (vals (frequencies wart)))))
  true
  false))) 

(defn two-pairs? [hand]
  (let [wart (map rank hand)]
  (if (and (= 2 (apply max (vals (frequencies wart)))) (= 2 (apply max (rest (vals (frequencies wart))))))
  true
  false))) 

(defn straight? [hand]
 (let [wart (map rank hand)
      zakres (range (apply min wart) (+ 1 (apply max wart)))]
 (if (and (= zakres (range (apply min wart) (+ 5 (apply min wart)))) (not (pair? hand)))
  true
  false)))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
