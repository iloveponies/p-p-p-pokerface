(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})


(defn rank [card]
  (let [[snd _] card]
    (if (Character/isDigit snd)
      (Integer/valueOf (str snd))
      (ranks snd))))

(defn suit [card]
  (let [[_ snd] card]
   (str snd)))


(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (if (and (= 2 (apply min (vals (frequencies (map rank hand)))))
           (three-of-a-kind? hand)) true false))

(defn two-pairs? [hand]
 (= 2(get (frequencies (vals (frequencies (map rank hand)))) 2)))


(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
        (cond
           (= sorted (range (apply min (map rank hand)) (+ 1(apply max (map rank hand))))) true
           (and (= 14 (apply max (map rank hand))) (= (range 1 6) (sort (replace {14 1} (map rank hand))))) true
           (= nil) false
        )))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
    (= nil) 0))

