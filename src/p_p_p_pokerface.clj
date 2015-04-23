(ns p-p-p-pokerface)


(defn suit [card]
  (let[[_ snd] card]
    (str snd)))



(defn rank [card]
  (let [[fst _] card]
    (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else (+ ({\T 9, \J 10, \Q 11, \K 12, \A 13} fst) 1))))

(defn turha-toisto-pois [hand luku]
  (= luku (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand ]
  (turha-toisto-pois hand 2))


(defn three-of-a-kind? [hand]
  (turha-toisto-pois hand 3))

(defn four-of-a-kind? [hand]
  (turha-toisto-pois hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
    (= [2 3] (sort(vals (frequencies (map rank hand)))) ))

(defn two-pairs? [hand]
  (cond
   (= [1 2 2] (sort (vals (frequencies (map rank hand))))) true
   (= [1 4] (sort (vals (frequencies (map rank hand))))) true
   :else false))



(defn straight? [hand]
    (let [uusi (sort (map rank hand))]
      (cond
       (= (sort (replace {14 1} uusi)) (seq [1 2 3 4 5])) true
       (= uusi (seq [2 3 4 5 6])) true
       (= uusi (seq [3 4 5 6 7])) true
       (= uusi (seq [4 5 6 7 8])) true
       (= uusi (seq [5 6 7 8 9])) true
       (= uusi (seq [9 10 11 12 13])) true
       (= uusi (seq [10 11 12 13 14])) true
       :else false)))


(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (cond
  (straight-flush? hand) 8
  (full-house? hand) 6
  (straight? hand) 4
  (flush? hand) 5
  (four-of-a-kind? hand) 7
  (three-of-a-kind? hand) 3
  (two-pairs? hand) 2
  (pair? hand) 1
   :else 0))
















