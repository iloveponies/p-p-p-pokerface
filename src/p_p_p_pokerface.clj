(ns p-p-p-pokerface)

(def letter-value {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
   (if (Character/isDigit fst)
     (Integer/valueOf (str fst))
     (letter-value fst))))

(defn suit [card]
  (let [[fst snd] card]
   (str snd)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (if (contains? (set (rank-frequencies hand)) 2 )
    true
    false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (rank-frequencies hand)) 3 )
    true
    false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (rank-frequencies hand)) 4 )
    true
    false))

(defn flush? [hand]
  (if (contains? (set (suit-frequencies hand)) 5 )
    true
    false))

(defn full-house? [hand]
  (if(and
      (pair? hand)
      (three-of-a-kind? hand))
    true
    false))

(defn two-pairs? [hand]
  (if (== (get (frequencies (rank-frequencies hand)) 1) 1)
    true
    false))

(defn straight-found? [ranks]
  (let [smallest (apply min ranks)]
    (if (or (= (range smallest (+ smallest 5)) (sort ranks))
            (= (range smallest (+ smallest 5)) (sort (replace {14, 1} ranks))))
      true
      false)))

(defn straight? [hand]
  (let [ranks (map rank hand)]
  (if (or (straight-found? ranks)
          (straight-found? (replace {14, 1} ranks)))
    true
    false)))



(defn straight-flush? [hand]
  (if (and
       (straight? hand)
       (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

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


