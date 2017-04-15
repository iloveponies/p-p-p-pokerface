(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (get {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14} r )))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (if (<= 2 (apply max (vals (frequencies (map rank hand)))))
  true
  false))

(defn three-of-a-kind? [hand]
   (if (<= 3 (apply max (vals (frequencies (map rank hand)))))
  true
  false))

(defn four-of-a-kind? [hand]
 (if (<= 4 (apply max (vals (frequencies (map rank hand)))))
  true
  false))

(defn flush? [hand]
  (if (= 1 (count (keys (frequencies (map suit hand)))))
    true
    false
    ))

(defn full-house? [hand]
  (if (= (seq [2 3]) (sort (vals (frequencies (map rank hand)))))
    true
    false
   ))

(defn two-pairs? [hand]
 (if  (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))
      true
      false
  ))


(defn straight? [hand]
(let [r-straight (fn[rhand] (if (= (sort rhand) (range (first (sort rhand)) (+ (first (sort rhand)) 5)))
                             true
                             false))]
(if ( or (r-straight (map rank hand)) (r-straight (replace { 14 1} (map rank hand))))
  true
  false
  )))

(defn straight-flush? [hand]
 (if (and (straight? hand) (flush? hand))
     true
     false))

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
