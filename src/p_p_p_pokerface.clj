(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (if (pair? hand) 
    (if (three-of-a-kind? hand)
      true
      false) 
    false))

(defn two-pairs? [hand]
  (cond
    (contains? (set (vals (frequencies (map rank hand)))) 4) true
    (= [1 2 2] (sort (vals (frequencies (map rank hand))))) true
    :else false))

(defn straight? [hand]
  (let [[fst snd trd frt fif] (sort (map rank hand))]
    (if (= (+ fst 1) snd) 
      (if (= (+ snd 1) trd) 
        (if (= (+ trd 1) frt) 
          (if (= (+ frt 1) fif) 
            true 
            (if (= fif 14) 
              (if (= fst 2) 
                true false) 
              false)) false) false) false)))

(defn straight-flush? [hand]
  (if (straight? hand) (if (flush? hand) true false) false))

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
