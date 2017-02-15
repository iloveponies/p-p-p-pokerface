(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (= rank \T) 10
      (= rank \J) 11
      (= rank \Q) 12
      (= rank \K) 13
      (= rank \A) 14
      :else (Integer/valueOf (str rank))
      )
    )
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (mapv(fn [i] (= 2 i)) (vals (frequencies (mapv rank hand))))) true)
  )

(defn three-of-a-kind? [hand]
  (contains? (set (mapv(fn [i] (= 3 i)) (vals (frequencies (mapv rank hand))))) true))

(defn four-of-a-kind? [hand]
  (contains? (set (mapv(fn [i] (= 4 i)) (vals (frequencies (mapv rank hand))))) true))

(defn flush? [hand]
  (apply = (map suit hand))
  )

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand))
  )

(defn two-pairs? [hand]
  (or (= (count (filterv (fn [i] (= 2 i)) (vals (frequencies (mapv rank hand))))) 2) (four-of-a-kind? hand))
  )

(defn straight? [hand]
  (let [values (sort (map rank hand))
        reverseValues (sort (replace {14, 1} values))
        i (apply min values)]
    (cond
      (= (range i (+ i 5)) values) true
      (= (range 1 6) reverseValues) true
      :else false))
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

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
    :else 0)
    )

; $(face)
