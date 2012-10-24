(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank) 
      (Integer/valueOf (str rank))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 1)
    true
    false
    ))

(defn three-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 2)
    true
    false
    ))

(defn four-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 3)
    true
    false
    ))

(defn flush? [hand]
  (if (> (apply max (vals (frequencies (map (fn [x] (suit x)) hand)))) 4)
    true
    false))

(defn full-house? [hand]
  (if (= (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 3)
    (if (= (apply min (vals (frequencies (map (fn [x] (rank x)) hand)))) 2)
	  true
      false)
    false))

(defn two-pairs? [hand]
  (let [ma (vals (frequencies (map (fn [x] (rank x)) hand)))
        c (count ma)]
    (if (and (= 2 (apply max ma)) (= c 3)) true 
      (if (= 4 (apply max ma)) true false))))

(defn lowace-straight? [hand]
  (let [mi (apply min (map (fn [x] (if (= 14 (rank x)) 1 (rank x) )) hand))
        ma (apply max (map (fn [x] (if (= 14 (rank x)) 1 (rank x) )) hand))
        diff (- ma mi)
        max_f (apply max (vals (frequencies (map (fn [x] (rank x)) hand))))]
    (if (and (= diff 4) (= max_f 1)) true false)))

(defn straight? [hand]
  (let [mi (apply min (map (fn [x] (rank x)) hand))
        ma (apply max (map (fn [x] (rank x)) hand))
        diff (- ma mi)
        max_f (apply max (vals (frequencies (map (fn [x] (rank x)) hand))))]
    (if (and (= diff 4) (= max_f 1)) true (lowace-straight? hand))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

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
   :else 0
   ))