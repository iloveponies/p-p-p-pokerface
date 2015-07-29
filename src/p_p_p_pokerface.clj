(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank) (Integer/valueOf (str rank))   (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 2 2]))

(defn straight? [hand]
  (let [pieni-suora (sort (replace {14 1} (map rank hand)))
        iso-suora (sort (map rank hand))
        alku (apply min (map rank hand))
        testisuora (range alku (+ alku 5))]
  (or (= pieni-suora (range 1 (+ 1 5))) (= iso-suora testisuora))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (and (= (apply max (vals (frequencies (map rank hand)))) 1) (not (flush? hand)) (not (straight? hand))))

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
         (high-card? hand) 0
         :else "virhe"))
