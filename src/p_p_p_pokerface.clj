(ns p-p-p-pokerface)

(def isot {"2" 2, "3" 3, "4" 4, "5" 5, "6" 6, "7" 7, "8" 8, "9" 9, "T" 10, "J" 11, "Q" 12, "K" 13, "A" 14})

(def pienet {"2" 2, "3" 3, "4" 4, "5" 5, "6" 6, "7" 7, "8" 8, "9" 9, "T" 10, "J" 11, "Q" 12, "K" 13, "A" 1})

(defn rank [card]
  (let [[r _] card]
    (get isot (str r))
  ))

(defn rank2 [card]
  (let [[r _] card]
    (get pienet (str r))
  ))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  ;  (if (contains? (set (keys (frequencies (vals (frequencies (map rank hand)))))) 2)
    ;    (> (get 
        ;        (set (keys (frequencies (vals (frequencies 
                  ;                  (map rank hand)))))) 
        ;        2) 1)
    ;    (contains? (set (vals (frequencies (map rank hand)))) 4))
  (and (or (four-of-a-kind? hand) (not (three-of-a-kind? hand))) (> (count hand) (+ 1 (count (set (map rank hand))))))
)

(defn iso-suora? [hand]
  (let [kortit (map rank hand)]
    (and 
      (== (+ (apply min kortit) 4) 
        (apply max kortit)) 
      (== (apply max (vals (frequencies kortit))) 
        1)
    )))

(defn pikku-suora? [hand]
  (let [kortit (map rank2 hand)]
    (and 
      (== (+ (apply min kortit) 4) 
        (apply max kortit)) 
      (== (apply max (vals (frequencies kortit))) 
        1)      
    )))

(defn straight? [hand]
  (or (iso-suora? hand) (pikku-suora? hand)))

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
