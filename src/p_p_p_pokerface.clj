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
  (if (and (= 2 (apply max (vals (frequencies wart)))) (= 2 (apply max (butlast(sort (vals (frequencies wart)))))))
  true
  false))) 

(defn straight? [hand]
 (let [wart (map rank hand)
      wartlow (replace {14 1} wart)
      wmin (apply min wart)
      wminlow (apply min wartlow)
      wmax (apply max wart)
      wmaxlow (apply max wartlow)
      zakres (range wmin (+ 1 wmax))
      zakreslow (range wminlow (+ 1 wmaxlow))
      ]
 (if (= wmax 14) 
  (do 
  (if (and (not (pair? hand)) (not (three-of-a-kind? hand)) (or 
  (= zakres (range wmin (+ 5 wmin))) 
  (= zakreslow (range wminlow (+ 5 wminlow)))))
  true
  false))
  (do (if(and (= zakres (range wmin (+ 5 wmin))) (not (pair? hand)))
  true
  false)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand) ))

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
  :otherwise 0)
)
