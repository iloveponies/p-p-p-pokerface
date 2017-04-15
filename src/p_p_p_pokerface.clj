(ns p-p-p-pokerface)

(def ranks {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [[rank suit]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get ranks rank)
    ) 
  )

(defn suit [[rank suit]]
  (str suit))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (set (vals(frequencies(map suit hand)))))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (== x 2)) (vals(frequencies(map rank hand)))))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand)) frst (first sorted)] 
   (or (= (range frst (+ frst 5)) sorted) 
       (= (range 1 6) (sort (replace {14 1} sorted))))))

  

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

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
