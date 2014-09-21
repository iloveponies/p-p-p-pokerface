(ns p-p-p-pokerface)

(defn rank [card]
  ;(cond ((Character/isDigit (get card 0))) ((Integer/valueOf (str (get card 0))))
   ;     :else  (get card 0)
  ;)
  (cond (Character/isDigit (get card 0)) (Integer/valueOf (str (get card 0)))
        :else  (get {\T 10, \J 11, \Q 12, \K 13, \A 14} (get card 0))
        )

)
(defn replaced [X]
  (get {\T 10, \J 11, \Q 12, \K 13, \A 14} X)
  )

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (cond (<= 2 (apply max (vals (frequencies (map rank hand))))) true
       :else false
   )
  )

(defn three-of-a-kind? [hand]
    (cond (<= 3 (apply max (vals (frequencies (map rank hand))))) true
       :else false
   )
  )

(defn four-of-a-kind? [hand]
    (cond (<= 4 (apply max (vals (frequencies (map rank hand))))) true
       :else false
   )
  )

(defn flush? [hand]
   (cond (= 5 (apply max (vals (frequencies (map suit hand))))) true
       :else false
   )
  )

(defn full-house? [hand]
  (= [2 3] (sort(vals(frequencies (map rank hand)))))
  )

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort(vals(frequencies (map rank hand)))))
      (= [1 4] (sort(vals(frequencies (map rank hand))))))
  )

(defn straight? [hand]
  (let [x  (sort (keys (frequencies (map rank hand))))]

    (cond (= [2 3 4 5 14] x) true
          :else  (and (= [1 1 1 1 1] (sort(vals(frequencies (map rank hand)))))
                     ; (not (= [5] (sort(vals(frequencies (map suit hand))))))
                      (every? #{1} (map - (rest x) x))
                  )
     )
  )
  )

(defn high-card? [hand]
  true)

(defn incrementing?
  [xs]
  (every? #(= % -1) (map #(apply - %) (partition 2 1 xs))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)
  )
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
   (high-card? hand) 0
   )
  )
