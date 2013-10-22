(ns p-p-p-pokerface)

(defn rank [card]
  (let [r (first card)]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\A 14, \K 13, \Q 12, \J 11, \T 10} r))))

(rank "AH")

(defn suit [card]
  (str (let [[_ snd] card]
         snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 2 2]))

(defn straight? [hand]
  (let [arvot (sort (map rank hand))]
  (or
   (= arvot (range (first arvot) (+ 5 (first arvot))))
   (let [arvot (sort (replace {14 1} (map rank hand)))]
   (= arvot (range (first arvot) (+ 5 (first arvot))))))))

(straight? ["2H" "3S" "6C" "5D" "4D"])
(straight? ["2D" "3D" "4D" "5D" "AD"])

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
