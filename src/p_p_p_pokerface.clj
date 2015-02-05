(ns p-p-p-pokerface)

; helper
(def rankmap {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn suit [card]
  (let [ [fst snd] card ]
    (str snd)))

(defn rank [card]
  (let [ [fst snd] card ]
    (if (Character/isDigit fst)
        (Integer/valueOf (str fst))
        (get rankmap fst))))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (not (four-of-a-kind? hand))
       (= 2 (count (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or
     (and (not (three-of-a-kind? hand) )
          (= 3 (count (vals (frequencies (map rank hand))))))
     (four-of-a-kind? hand)))


(defn straight? [hand]
  (let [hvals (map rank hand)
        hvalsb (replace {1 14, 14 1} hvals)
        ]
    (or
       (= (sort hvals) (range (apply min hvals) (+ (apply min hvals) 5)))
       (= (sort hvalsb) (range (apply min hvalsb) (+ (apply min hvalsb) 5))))))


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
