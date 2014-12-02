(ns p-p-p-pokerface)

(defn rank [card]
  (let [[nm _] card]
    (if (Character/isDigit nm)
        (Integer/valueOf (str nm))
        (get {\T 10, \J 11, \Q 12, \K 13, \A 14} nm))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn how-many-of-kind? [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (<= 2 (how-many-of-kind? hand)))

(defn three-of-a-kind? [hand]
  (<= 3 (how-many-of-kind? hand)))

(defn four-of-a-kind? [hand]
  (<= 4 (how-many-of-kind? hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [[num nam] (vals (frequencies(map rank hand)))]
    (and (or (= 3 num) (= 3 nam)) (or (= 2 num) (= 2 nam)))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [hnd (sort (map rank hand))
      hnd2 (sort (replace {14 1} hnd))
      smlst (apply min hnd)]
  (or (= hnd (range smlst (+ smlst 5)))
      (= hnd2 (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

; O______o
