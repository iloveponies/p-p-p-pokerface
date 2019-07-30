(ns p-p-p-pokerface)

(def replacements {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
    (replacements fst)))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
   (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
   (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (> (apply max (vals (frequencies (map suit hand)))) 4))

(defn full-house? [hand]
   (and ( = (apply max (vals (frequencies (map rank hand)))) 3)
        ( = (apply min (vals (frequencies (map rank hand)))) 2)))
(comment (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) '(1 2 2))
      (four-of-a-kind? hand)))

(defn straight? [hand]
   (let [ranks (map rank hand)

        sortedranks1 (sort ranks)
        sortedranks2 (sort (replace {14 1} ranks))

        minrank1 (apply min sortedranks1)
        minrank2 (apply min sortedranks2)

        straighthouse1 (range minrank1 (+ minrank1 5))
        straighthouse2 (range minrank2 (+ minrank2 5))]
       (or (= straighthouse1 sortedranks1)
           (= straighthouse2 sortedranks2))
    ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand)  6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
